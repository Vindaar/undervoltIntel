# a little tool to undervolt an intel CPU (haswell and newer)

import docopt
import strutils, math, osproc, strformat

const doc = """
A small tool to undervolt an Intel (Haswell and newer) CPU on linux.
It automatically applies the same undervolt to the CPU core and
CPU cache.
NOTE: values given are always interpreted as negative integers! To overvolt
your CPU, rewrite the code...

Usage:
  undervoltIntel (--CPU=VAL1 --iGPU=VAL2 | --read) [options]

Options:
  --CPU=VAL1      The undervolt to be applied to CPU core and cache in mV.
  --iGPU=VAL2     The undervolt to be applied to the integrated GPU in mV.
  --read          If called with read flag, will only output current msr
                  register values.
  -h, --help      Show this help.
"""

type
  # enum for the different planes
  PlaneKind = enum
    pkCpuCore, pkIGpu, pkCpuCache, pkSystemAgent, pkAnalogIO, pkDigitalIO

  ReadWriteKind = enum
    rwRead = "0"
    rwWrite = "1"

## The final command consists of the followoing pieces
## constant	plane index	constant	write/read	offset
## 80000	0	        1	        1       	F9A00000

const MsrRegister = "0x150"
const ConstantPrefix = "80000"
const ConstantMid = "1"
# "value" to be used if we read a register (write/read == 0)
const ReadValue = "00000000"

const WriteProg = "wrmsr"
const ReadProg = "rdmsr"

proc insertOrCheckMsrModule() =
  ## check whether MSR kernel module inserted, if not do so
  var
    outp: string
    errC: int
  (outp, errC) = execCmdEx("lsmod | grep msr")
  if outp.len == 0:
    (outp, errC) = execCmdEx("sudo modprobe msr")
    if errC != 0:
      echo &"Could not insert `msr` kernel module! errC: {errC}"
    else:
      (outp, errC) = execCmdEx("lsmod | cut -d' ' -f1 | grep msr")
      if outp.strip != "msr":
        quit(&"Did not successfully insert `msr` module! Output: {outp.strip}")
      else:
        echo &"succesfully inserted `msr` module: {outp}"

func convertToHexStr(x: int): string =
  ## calculates the hex string from the undervolt in mV
  ## see e.g.: https://wiki.manjaro.org/index.php?title=Undervolt_intel_CPU
  doAssert x < 256
  # always take the negative
  let xNeg = min(x, -x).float
  # scale and round to nearest int
  let xScaled = (xNeg * 1.024).round.int
  # invert all bits, add 1 to get "negative"
  let xInvU8 = ((abs(xScaled)).uint8 xor 0xFF'u8) + 1
  # define the mask needed
  const mask = 0xE0000000'u32
  # conert xInvU8 to uint32 and bitshift to left by 21
  let xShift = xInvU8.uint32 shl 21
  result = toHex(xShift or mask)

func hexToMilliVolt(x: string): int =
  ## calculates the mV value from the hex string. Inverse of above
  let xInt = (&"0x{x}").parseHexInt.uint32
  let xShift = xInt shr 21
  let xInvU8 = ((xShift - 1).uint8 xor 0xFF'u8)
  result = (xInvU8.float / 1.024).round.int

func genCmdString(offset: string, writeRead: ReadWriteKind,
                  plane: PlaneKind): string =
  ## generates the final command string from the value and the plane
  result.add "0x" & ConstantPrefix
  result.add $(plane.int)
  result.add ConstantMid
  result.add $writeRead
  result.add offset

func writeCall(cmd: string): string =
  ## returns the write call of the given command
  result = &"sudo {WriteProg} {MsrRegister} {cmd}"

func readCall(): string =
  ## returns the read call
  result = &"sudo {ReadProg} {MsrRegister}"

proc execCommand(readWrite: ReadWriteKind, cmd = ""): string =
  ## executes the system call
  var cmdString = ""
  case readWrite
  of rwRead:
    cmdString = readCall()
  of rwWrite:
    cmdString = writeCall(cmd)
  let (outp, errC) = execCmdEx(cmdString)
  if errC != 0:
    echo &"Error occured during call to: {cmdString}"
  result = outp

proc readPlane(plane: PlaneKind): string =
  ## performs a read of the given plane and returns the value
  # first write empty command to the plane
  let cmd = genCmdString(ReadValue, rwRead, plane)
  # perform a write
  discard execCommand(rwWrite, cmd)
  result = execCommand(rwRead).strip.normalize

when isMainModule:
  let args = docopt(doc)
  var
    cpuVal: int
    gpuVal: int
    outputPrefix = ""

  let readOnly = if $args["--read"] == "true": true else: false
  if not readOnly:
    outputPrefix = "\t"
    cpuVal = ($args["--CPU"]).parseInt
    gpuVal = ($args["--iGPU"]).parseInt

  # make sure conversion to hex works and command creation
  # works as intended!
  let testVal = convertToHexStr(175)
  doAssert testVal == "E9A00000"
  doAssert genCmdString(testVal, rwWrite, pkCpuCore) == "0x80000011E9A00000"
  doAssert genCmdString(testVal, rwWrite, pkIGpu)    == "0x80000111E9A00000"
  doAssert genCmdString(ReadValue, rwRead, pkIGpu)   == "0x8000011000000000"

  # before we do anything, check if the MSR kernel module is inserted
  insertOrCheckMsrModule()

  var
    cpu: string
    gpu: string
  if not readOnly:
    cpu = convertToHexStr(cpuVal)
    gpu = convertToHexStr(gpuVal)

    # create the command strings
    let cpuCore = genCmdString(cpu, rwWrite, pkCpuCore)
    let cpuCache = genCmdString(cpu, rwWrite, pkCpuCache)
    let iGpu = genCmdString(gpu, rwWrite, pkIGpu)

    discard execCommand(rwWrite, cpuCore)
    discard execCommand(rwWrite, cpuCache)
    discard execCommand(rwWrite, iGpu)

  let
    readCore = readPlane(pkCpuCore)
    readCache = readPlane(pkCpuCache)
    readIGpu = readPlane(pkIGpu)

  if not readOnly:
    doAssert readCore == cpu.normalize, &" was {readPlane(pkCpuCore)}"
    doAssert readCache == cpu.normalize, &" was {readPlane(pkCpuCache)}"
    doAssert readIGpu == gpu.normalize, &" was {readPlane(pkIGpu)}"
    echo "Successfully set:"

  echo &"{outputPrefix}CPU core: -{readCore.hexToMilliVolt} mV"
  echo &"{outputPrefix}CPU cache: -{readCache.hexToMilliVolt} mV"
  echo &"{outputPrefix}iGPU: -{readIGpu.hexToMilliVolt} mV"
