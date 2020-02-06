# cahp-diamond

4th generation implementation of VSP Processor

# Build
## Dependency
please install below packages.
```
scala
sbt
yosys
```
## Procedure
run below command
```
sbt run
```

Then, `VSPCoreNoRAMROM.v` `VSPCoreNoROM.v` is created. This file is verilog format.

Next, Synthesize verilog format file with yosys.

If you want to generate vsp-core without RAM, please exec below command.
```
yosys build-no-ram-rom.ys
```
`vsp-core-no-ram-rom.json` will be created.

If you want to generate vsp-core with RAM, please exec below command.
```
yosys build-no-rom.ys
```
`vsp-core-no-rom.json` will be created.

Next, we need to convert this file to orignal format file for Iyokan with Iyokan-L1
