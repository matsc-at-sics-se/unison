#!/usr/bin/env python
#
#  Main authors:
#    Jacob Kimblad <jacob.kimblad@ri.se>
#
#  This file is part of Unison, see http://unison-code.github.io
#
#  Copyright (c) 2018, RISE SICS AB
#  All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions are met:
#  1. Redistributions of source code must retain the above copyright notice,
#     this list of conditions and the following disclaimer.
#  2. Redistributions in binary form must reproduce the above copyright notice,
#     this list of conditions and the following disclaimer in the documentation
#     and/or other materials provided with the distribution.
#  3. Neither the name of the copyright holder nor the names of its contributors
#     may be used to endorse or promote products derived from this software
#     without specific prior written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
#  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
#  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
#  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
#  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
#  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
#  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
#  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
#  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#  POSSIBILITY OF SUCH DAMAGE.
#

import json
import pyparsing as pp
import sys

#To envoke this program correctly, execute:
#llvm-tblgen /.../llvm-6.0.0.src/lib/Target/X86/X86.td -InstrInfo -I /.../llvm-6.0.0.src/include -I /.../llvm-6.0.0.src/lib/Target/X86 | ./tablegen-instruction-parser.py
#With local paths to llvm 6.0.0 (non-compiled version)

# Main
def main():
    tablegenDefs = extractInstructions(readIn())
    print(json.dumps(tablegenDefs, indent=4))

# Extract each instruction outputed by tablegen and its respective SchedRW definition
def extractInstructions(tablegenDefs):
    extractedInstructions = []
    currentInstruction = ""
    currentSchedRW = None
    instructionDefined = False
    schedRWDefined = False
    firstInstruction = True
    instruction = {
            'Instruction' : None,
            'SchedRW' : None
            }
    for line in tablegenDefs:
        #Dictionary to hold definitions
        #Current line is a def of an instruction
        if line.find("def") >= 0:

            if firstInstruction:
                currentInstruction = line.split(" ")[1]
                instructionDefined = True
                firstInstruction = False
                continue

            #Found a new instruction, with earlier instruction having a defined SchedRW
            if schedRWDefined:
                instruction['Instruction'] = currentInstruction
                instruction['SchedRW'] = currentSchedRW
                extractedInstructions.append(instruction.copy())
                currentInstruction = line.split(" ")[1]
                currentSchedRW = None
                instructionDefined = True
                schedRWDefined = False

            #Found a new instruction, with earlier instruction not having a defined SchedRW
            else:
                currentInstruction = line.split(" ")[1]
                instructionDefined = True
                schedRWDefined = False

        #Current line is a schedRW that belongs to an instruction
        if line.find("list<SchedReadWrite> SchedRW") >= 0:

            #Found a second schedRW for the same instruction
            if schedRWDefined:
                raise Exception("Error while parsing! Found two defined SchedRW for same instruction: " + currentInstruction)

            #Found a schedRW that belongs to an instruction
            if instructionDefined:
                currentSchedRW = line[33:-1]
                schedRWDefined = True
                instructionDefined = False

            # Error, this is undefined behaviour as we found a SchedRW that does not belong to an instruction
            else:
                raise Exception("Error while parsing! Found a dangling SchedRW not belonging to an instruction:" + currentInstruction) 

    #Append last instruction in file as well
    if currentInstruction and currentSchedRW:
        instruction['Instruction'] = currentInstruction
        instruction['SchedRW'] = currentSchedRW
        extractedInstructions.append(instruction.copy())


    return extractedInstructions

# Read from output of tablegen and extract all the defs of instructions
def readIn():
    inLines = sys.stdin.readlines()
    defsReached = False
    defs = []

    #Find the defs from the output, which is all we want
    for line in inLines:
        line = line.strip('\n')
        #Look for specific line, which signifies the start of definitions
        if (not defsReached) and (line.find("------------- Defs -----------------") >= 0):
            defsReached = True
        elif defsReached:
            defs.append(line)

    return defs
    
if __name__ == '__main__':
    main()

