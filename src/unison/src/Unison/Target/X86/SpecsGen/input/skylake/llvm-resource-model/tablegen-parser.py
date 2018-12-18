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
import yaml
# import pyparsing as pp
import sys
# import logging, sys
    # logging.debug("splitSchedRW %s", str)

# Main
def main():
    # logging.basicConfig(stream=sys.stderr, level=logging.DEBUG)
    iset = yaml.load(open(sys.argv[1], 'r'))
    selected = {}
    for insn in iset['instruction-set'][0]['instructions']:
        selected[insn['id']] = True
    del iset
    lines = readIn()
    defs, undefs, usedItineraries = extractInstructions(lines, selected)
    schedAliases = extractSchedAliases(lines)
    resGroups = extractResourceGroups(lines, schedAliases, usedItineraries)
    output = {
            'ResourceGroups': resGroups,
            'DefinedInstructions': defs,
            'UndefinedInstructions': undefs
            }
    print(json.dumps(output, indent=4))

def splitSchedRW(str):
    str = str.strip("[]")
    if str.find(",") >= 0:
        return str.split(",")[0], True
    else:
        return str, False
        
# Extract each instruction output by tablegen and its respective SchedRW definition
def extractInstructions(lines, selected):
    definedInstructions = []
    undefinedInstructions = []
    usedItineraries = {}
    currentInstruction = ""
    currentSchedRW = None
    instructionDefined = False
    schedRWDefined = False
    firstInstruction = True
    instruction = {
            'Instruction' : None,
            'ResourceGroup' : None,
            'ReadAdvance' : None
            }
    for line in lines:
        if line.find("def") >= 0:

            if firstInstruction:
                currentInstruction = line.split(" ")[1]
                instructionDefined = True
                firstInstruction = False
                continue

            #Found a new instruction, with earlier instruction having a defined SchedRW
            if schedRWDefined:
                if currentSchedRW == "?" or not (currentInstruction in selected):
                    undefinedInstructions.append({'Instruction' : currentInstruction})
                else:
                    rg, adv = splitSchedRW(currentSchedRW)
                    instruction['Instruction'] = currentInstruction
                    instruction['ResourceGroup'] = rg
                    instruction['ReadAdvance'] = adv
                    usedItineraries[rg] = True
                    definedInstructions.append(instruction.copy())
                    
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
        if currentSchedRW == "?" or not selected[currentInstruction]:
            undefinedInstructions.append({'Instruction' : currentInstruction})
        else:
            rg, adv = splitSchedRW(currentSchedRW)
            instruction['Instruction'] = currentInstruction
            instruction['ResourceGroup'] = rg
            instruction['ReadAdvance'] = adv
            usedItineraries[rg] = True
            definedInstructions.append(instruction.copy())

    return definedInstructions, undefinedInstructions, usedItineraries

def makeStringList(str):
    str = str.strip("[];")
    if len(str)>0:
        return str.split(", ")
    else:
        return []

def makeIntList(str):
    str = str.strip("[];")
    if len(str)>0:
        return list(map(int, str.split(", ")))
    else:
        return []

def makeInt(str):
    str = str.strip(";")
    return int(str)

# Extract each resource group definition
def extractResourceGroups(lines, schedAliases, usedItineraries):
    resourceGroups = []
    inside = False
    latency = False
    resourceGroup = {
        'Name' : None,
        'Latency' : None,
        'Resources' : None,
        'ResourceCycles' : None
        }

    for line in lines:
        if line.find("def") == 0 and (line.find("// SchedReadWrite") >= 0 or line.find("// ProcWriteResources") >= 0):
            inside = True
            resourceGroup['Name'] = line.split(" ")[1]
        elif inside and latency and line.find("}") == 0:
            name = resourceGroup['Name']
            if name in usedItineraries:
                resourceGroups.append(resourceGroup.copy())
            if name in schedAliases:
                for alias in schedAliases[name]:
                    if alias in usedItineraries:
                        resourceGroup['Name'] = alias
                        resourceGroups.append(resourceGroup.copy())
            inside = False
            latency = False
        elif line.find("}") == 0:
            inside = False
        elif inside and line.find("ProcResources = ") >= 0:
            resourceGroup['Resources'] = makeStringList(line.split(" = ")[1])
        elif inside and line.find("ResourceCycles = ") >= 0:
            cycles = makeIntList(line.split(" = ")[1])
            if cycles == []:
                cycles = [1 for x in resourceGroup['Resources']]
            resourceGroup['ResourceCycles'] = cycles
        elif inside and line.find("Latency = ") >= 0:
            resourceGroup['Latency'] = makeInt(line.split(" = ")[1])
            latency = True
        elif inside and line.find("WriteType = ") >= 0:
            resourceGroup['Name'] = line.split(" = ")[1].strip(";")
    # add ad-hoc for pseudos
    resourceGroups.append({'Name' : "WriteFPUSH32", 'Latency' : 3, 'Resources' : ["SKLPort0156"], 'ResourceCycles' : [3]})
    return resourceGroups

# Extract each resource group alias
def extractSchedAliases(lines):
    schedAliases = {}
    inside = False
    match = None
    alias = None

    for line in lines:
        if line.find("def") == 0 and line.find("// SchedAlias") >= 0:
            inside = True
        elif inside and line.find("}") == 0:
            if alias in schedAliases:
                schedAliases[alias].append(match)
            else:
                schedAliases[alias] = [match]
            inside = False
        elif inside and line.find("MatchRW = ") >= 0:
            match = line.split(" = ")[1].strip(";")
        elif inside and line.find("AliasRW = ") >= 0:
            alias = line.split(" = ")[1].strip(";")

    return schedAliases

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

