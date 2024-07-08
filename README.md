# resource-function-COMMAND-LINE-PARSE

## Description 
Parses the a command returning a Dataset of all flags present in the command and their associated parameters

## Usage
| Usage | Description |
| ----------- | ----------- |
| `CommandLineParse[]` | Parses flags and parameters in $CommandLine into a Dataset |
| `CommandLineParse["cmd"]` | Parses flags and parameters in cmd string into a Dataset |
| `CommandLineParse["flag"]` | Returns parameters associated with flag in $CommandLine |
| `CommandLineParse["cmd", "flag"]` | Returns parameters associated with flag in cmd |
| `CommandLineParse[spec, {"flag1", "flag2", ...}]` | Returns parameters associated with the flags in the flag list |
