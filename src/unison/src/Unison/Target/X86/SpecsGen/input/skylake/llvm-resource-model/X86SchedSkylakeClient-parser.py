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

import sys
import json
import yaml
import pyparsing as pp
import re

def main():

    #Get all instructions defined in unison
    unisonInstructions = getUnisonInstructions()

    #Parser to find each basic WriteRes def with no additional attributes
    writeResDefParser = getWriteResDefParser()
    #Parser to find each WriteRes def with additional attributes (latency, resourcecycles etc.)
    writeResVerboseDefParser = getWriteResVerboseDef()
    #Parser to find each SKLWriteResPair def
    sklWriteResPairDefParser = getSklWriteResPairDefParser()
    #Parser to find each regex-instructions that belongs to a SKLWriteResGroup
    llvmInstructionParser = getLlvmInstructionParser()
    #Parser to find each SKLWriteResGroup definition
    sklWriteResGroupDefParser = getSklWriteResGroupDefParser()

    #Open LLVM-tablegen file defining skylake resources
    schedSkylakeClientTD = open(sys.argv[2]).read()

    #Find all WriteRes defs
    writeResDefs = getWriteResDefs(writeResDefParser, schedSkylakeClientTD)
    #Find all verbose WriteRes defs
    writeResVerboseDefs = getWriteResVerboseDefs(writeResVerboseDefParser, schedSkylakeClientTD)
    #Find all SKLWriteResPair defs
    sklWriteResPairDefs = getSklWriteResPairDefs(sklWriteResPairDefParser, schedSkylakeClientTD)
    #Find all SKLWriteResGroup defs
    sklWriteResGroupDefs = getSklWriteResGroupDefs(sklWriteResGroupDefParser, schedSkylakeClientTD)
    #Find all instructions defined for skylake by llvm
    llvmInstructions = getLlvmInstructions(llvmInstructionParser, schedSkylakeClientTD)

    # Find out which unison instructions has a matching regular-expression defined in llvm .td
    matchings = regexMatching(unisonInstructions, llvmInstructions)

    #Open file that contains output from tablegen
    tablegenOutput = json.load(open(sys.argv[4]))
    # Try and match all remaining instructions, that are not matched with any resource group, with what their schedRWGroups are defined as from the output of tablegen
    schedRWMatchings = getSchedRWMatchings(matchings['Unmatched'], tablegenOutput)
        
    #Save all defined resource groups
    resourceGroups = []
    for group in sklWriteResPairDefs + sklWriteResGroupDefs + writeResVerboseDefs + writeResDefs:
        resourceGroups.append(group['Name'])

    #Remove undefined resourcegroups from each defined instruction
    undefinedSchedRWGroup = []
    resourceGroupTuples = []
    for instruction in list(schedRWMatchings['Matched']):
        tempInstruction = removeUndefinedResourceGroups(instruction, resourceGroups)
        #Instruction had no defined resource group for skylake, so its resource usage is undefined
        if not tempInstruction['ResourceGroup']:
            undefinedSchedRWGroup.append({'Instruction': tempInstruction['Instruction']})
            schedRWMatchings['Matched'].remove(instruction)
        #Instruction has more than a singled defined resource group
        elif len(tempInstruction['ResourceGroup']) > 1:
            resourceGroupTuples.append(tempInstruction['ResourceGroup'])
            tempInstruction['ResourceGroup'] = "".join(tempInstruction['ResourceGroup'])
        #Instruction has a single defined resource group
        else:
            # Transform from list to single element
            tempInstruction['ResourceGroup'] = tempInstruction['ResourceGroup'][0]
            instruction = tempInstruction

    #Some instructions uses several resource-groups, so we create custom combined resourcegroups here.
    # Currently, the only collection of resource groups used by instructions are "WriteALULd" and "WriteRMW"
    # which create the combined resource "WriteALULdWriteRMW"
    definedResourceGroups = sklWriteResPairDefs + sklWriteResGroupDefs + writeResVerboseDefs + writeResDefs
    combinedResourceGroups = []
    for resourceGroups in set(tuple(row) for row in resourceGroupTuples):
        tempResource = combineResourceGroups(resourceGroups, definedResourceGroups)
        combinedResourceGroups.append(tempResource)

    definedResourceGroups.extend(combinedResourceGroups)

    #Load instructions that have been manually mapped to resource groups in an external file
    customInstructions = getCustomInstructions()
    undefinedInstructions = schedRWMatchings['Unmatched'] + undefinedSchedRWGroup
    #Remove manually defined instructions from the list of undefined instructions
    for instruction in customInstructions:
        undefinedInstructions[:] = [d for d in undefinedInstructions if d.get('Instruction') != instruction['Instruction']]
    
    # Check which instructions have defined ReadAfterLd, which means they have 5 cycles until the operand is fetched from memory and the instruction can actually be issued
    definedInstructions = matchings['Matched'] + schedRWMatchings['Matched'] + customInstructions
    for instruction in list(definedInstructions):
        if checkReadAdvance(tablegenOutput, instruction['Instruction']):
           instruction['ReadAdvance'] = True 
        else:
           instruction['ReadAdvance'] = False 

    #Format the output and print json (indent=4 enables pretty print)
    output = {
            'ResourceGroups': definedResourceGroups,
            'DefinedInstructions': definedInstructions,
            'UndefinedInstructions': undefinedInstructions,
            }
    print(json.dumps(output, indent=4))

    # Uncomment to print number of instructions NOT mapped to a resource group
    # print("unmatched: " + str(len(output['UndefinedInstructions'])))
    # Uncomment to print number of instructions mapped to a resource group
    # print("matched: " + str(len(output['DefinedInstructions'])))

# Check which instructions have defined ReadAfterLd, which means they have 5 cycles until the operand is fetched from memory and the instruction can actually be issued
def checkReadAdvance(tablegenOutput, instruction):
    match = list(filter(lambda schedRW : schedRW['Instruction'] == instruction, tablegenOutput))
    resourceGroups = []
    if match and match[0]['SchedRW'] != '?':
        resourceGroups = match[0]['SchedRW'].strip("[").strip("]").replace(" ", "").split(",")

    readAfterLdCount = 0
    for resourceGroup in resourceGroups:
        if resourceGroup == "ReadAfterLd":
            readAfterLdCount += 1

    if readAfterLdCount > 0:
        return True
    
    return False

#Get the SchedRW groups that belongs to each instruction passed as argument
def getSchedRWMatchings(instructions, tablegenOutput):

    matches = {
            'Matched': [],
            'Unmatched': []
            }

    for data in instructions:
        instruction = data['Instruction']
        match = list(filter(lambda schedRW : schedRW['Instruction'] == instruction, tablegenOutput))
        if match and match[0]['SchedRW'] != '?':
            matching = {
                    'Instruction': instruction,
                    'ResourceGroup': match[0]['SchedRW'].strip("[").strip("]").replace(" ", "").split(",")
                    }
            matches['Matched'].append(matching)

        else:
            matches['Unmatched'].append(data)

    return matches

#Some instructions does not have a given resourcegroup and have instead been manually mapped to a resource group, so we fetch them from that input file to include in the output
def getCustomInstructions():
    data = json.load(open(sys.argv[3]))
    return data['ManualMapping']

#Fetch all instructions defined for unison in x86.yaml
def getUnisonInstructions():
    data = yaml.safe_load(open(sys.argv[1], 'r'))

    instructions = []
    for instruction in data['instruction-set'][0]['instructions']:
        instructions.append(instruction['id'])

    return instructions

#Combines several defined resource groups into a single one
def combineResourceGroups (resourceGroupNames, definedResourceGroups):
    tempResourceGroup = {
            "Name" : "",
            "Latency" : 0,
            "Resources" : [],
            "ResourceCycles" : [],
            }

    for resourceGroupName in resourceGroupNames:
        tempResourceTuple = next(item for item in definedResourceGroups if item['Name'] == resourceGroupName)
        tempResourceGroup['Name'] += resourceGroupName 
        #Check if new largest latency
        if tempResourceGroup['Latency'] < tempResourceTuple['Latency']:
            tempResourceGroup['Latency'] = tempResourceTuple['Latency']
        n = 0
        while n < len(tempResourceTuple['Resources']):
            #Resource is not yet defined
            if tempResourceTuple['Resources'][n] not in tempResourceGroup['Resources']:
                tempResourceGroup['Resources'].append(tempResourceTuple['Resources'][n])
                tempResourceGroup['ResourceCycles'].append(tempResourceTuple['ResourceCycles'][n])

            #Resource is defined, so we need to add resource cycles to existing
            else:
                resourceIndex = tempResourceGroup['Resources'].index(tempResourceTuple['Resources'][n])
                tempResourceGroup['ResourceCycles'][resourceIndex] += tempResourceGroup['ResourceCycles'][n]

            n += 1

    return tempResourceGroup

#Removes undefined resouorcegroups from an instruction
def removeUndefinedResourceGroups (instruction, resourceGroups):
    definedResources = []
    #Get resource groups that are part of the ones defined for skylake AND are part of the instruction
    for resource in instruction['ResourceGroup']:
        if resource in resourceGroups:
            definedResources.append(resource)

    instruction['ResourceGroup'] = sorted(definedResources)
    return instruction

#Parser to find each basic WriteRes def with no additional attributes
def getWriteResDefParser():
    return pp.Suppress("def : WriteRes<") + pp.SkipTo(",")("Name")  + pp.SkipTo(">")("Resources") + pp.Suppress(">") + ~pp.Suppress("{") 

#Parser to find each WriteRes def with additional attributes (latency, resourcecycles etc.)
def getWriteResVerboseDef():
    return pp.Suppress("def : WriteRes<") + pp.SkipTo(",")("Name")  + pp.SkipTo(">")("Resources") + pp.Suppress(">") + pp.Suppress("{")  + pp.SkipTo("}")("Data") + pp.Suppress("}")

#Parser to find each SKLWriteResPair def
def getSklWriteResPairDefParser():
    return pp.Suppress("defm : SKLWriteResPair<") + pp.SkipTo(",")("Name") + pp.Suppress(",") +  pp.SkipTo(",")("Resources") + pp.Suppress(",") + pp.SkipTo(">")("Latency")

#Parser to find each regex-instructions that belongs to a SKLWriteResGroup
def getLlvmInstructionParser():
    writeResGroup = pp.Word(pp.alphanums)
    instRegex = pp.SkipTo("\"")
    return pp.Suppress("def: InstRW<[") + writeResGroup("ResourceGroup") + pp.Suppress("], (instregex \"") + instRegex("Regex") + pp.Suppress("\")>;")
    
#Parser to find each SKLWriteResGroup definition
def getSklWriteResGroupDefParser():
    writeResGroup = pp.Word(pp.alphanums)
    resources = pp.SkipTo("]")
    latency = pp.Word(pp.nums)
    microOps = pp.Word(pp.nums)
    resourceCycles = pp.SkipTo("]")
    return pp.Suppress("def ") + writeResGroup("SKLWriteResGroup") + pp.Suppress(": SchedWriteRes<[") + resources("Resources") + pp.Suppress(pp.restOfLine) + (
            pp.Suppress("let Latency = ") + latency("Latency")  + pp.Suppress(pp.restOfLine) +
            pp.Suppress("let NumMicroOps = ") + microOps("NumMicroOps") + pp.Suppress(pp.restOfLine) + 
            pp.Suppress("let ResourceCycles = [") + resourceCycles("ResourceCycles") + pp.Suppress(pp.restOfLine)
        )

#Find all WriteRes defs
def getWriteResDefs(writeResDef, schedSkylakeClientTD):
    writeResDefs = []
    for writeRes in writeResDef.searchString(schedSkylakeClientTD):
        #Pretty up the parsed data
        tempDict = {
                "Name": writeRes['Name'],
                "Latency": 1,
                "Resources": writeRes['Resources'].strip(",").strip().strip("[").strip("]").replace(" ", "").split(","),
                "ResourceCycles": [],
                }
        #Check if Resources contains only an empty element and should be completely empty instead
        if len(tempDict['Resources'][0]) == 0:
            tempDict['Resources'] = []
        #Set one resource cycle for each resource (implicit in .td-file)
        else:
            for resource in tempDict['Resources']:
                tempDict['ResourceCycles'].append(1)

        writeResDefs.append(tempDict)
    return writeResDefs

#Find all verbose WriteRes defs
def getWriteResVerboseDefs(writeResVerboseDef, schedSkylakeClientTD):
    writeResVerboseDefs = []
    for writeRes in writeResVerboseDef.searchString(schedSkylakeClientTD):
        #Pretty up the parsed data
        writeResDict = writeRes.asDict()
        tempDict = {
                'Name' : writeRes['Name'],
                'Latency' : 1,
                'Resources' : writeRes['Resources'].strip(",").strip().strip("[").strip("]").replace(" ", "").split(","),
                'ResourceCycles' : []
                }
        
        #Go through each line of data that belongs to the WriteRes
        tempData = writeResDict['Data'].strip().split("\n")
        for data in tempData:
            #Remove comments that may have been parsed
            data = data.split("//")[0]
            if data:
                data = data.strip(";").strip()
                if data.find("Latency") >= 0 :
                    #Latency is not an int
                    if isNumber(data.split("=")[1].strip()):
                        tempDict['Latency'] = int(data.split("=")[1].strip())
                    #Latency is NaN (This happens due to wrongfully parsing once inside a specially defined function)
                    else:
                        tempDict['Latency'] = 1
                elif data.find("ResourceCycles") >= 0:
                    tempData = data.split("=")[1].strip().replace(" ", "").strip("[").strip("]").split(",")
                    #Check all list items are numericals
                    tempIntData = [s for s in tempData if s.isdigit()]
                    tempDict['ResourceCycles'] = list(map(int, tempIntData))

        #Check if Resources contains only an empty element and should be completely empty instead
        if len(tempDict['Resources'][0]) == 0:
            tempDict['Resources'] = []
        #Check if resourceCycles are not defined although resources are (resource-group "WriteLoad" suffers from this)
        if len(tempDict['ResourceCycles']) == 0:
            for resource in tempDict['Resources']:
                tempDict['ResourceCycles'].append(1)

        writeResVerboseDefs.append(tempDict)
        
    return writeResVerboseDefs

#Find all SKLWriteResPair defs
def getSklWriteResPairDefs(sklWriteResPairDef, schedSkylakeClientTD):
    sklWriteResPairs = []
    for sklWriteResPair in sklWriteResPairDef.searchString(schedSkylakeClientTD):
        tempDict = {
                'Name' : sklWriteResPair['Name'],
                'Latency' : int(sklWriteResPair['Latency']),
                'Resources' : sklWriteResPair["Resources"].strip(",").strip().split(","),
                #RecourceCycles is implicit for the current version of the .td file, but may have to be updated if the file is changed to a later version of llvm than 6.0.0
                'ResourceCycles' : []
                }
        #Set one resource cycle for each resource (implicit in .td-file)
        #Check if Resources is empty and should be empty instead of containing just double quotations("")
        if len(tempDict['Resources'][0]) == 0:
            tempDict['Resources'] = []
        #Set one resource cycle for each resource (implicit in .td-file)
        else:
            for resource in tempDict['Resources']:
                tempDict['ResourceCycles'].append(1)
        sklWriteResPairs.append(tempDict)

        # Defined the corresponding resource with a folded load
        resourcesFolded = list(tempDict['Resources'])
        added = False

        #Check if resource-group already uses port23
        if 'SKLPort23' not in tempDict['Resources']:
            resourcesFolded.append('SKLPort23')
            added = True
        tempDictFolded = {
                'Name' : sklWriteResPair['Name'] + 'Ld',
                'Latency' : int(sklWriteResPair['Latency']) + 5,
                'Resources' : resourcesFolded,
                #RecourceCycles is implicit for the current version of the .td file, but may have to be updated if the file is changed to a later version of llvm than 6.0.0
                'ResourceCycles' : []
                }

        #Add resource cycles for port23
        # if added:
            # tempDictFolded['ResourceCycles'] = tempDict['ResourceCycles'].append(1)
        
        #Add implicitly defined resource cycles
        for resource in tempDictFolded['Resources']:
            tempDictFolded['ResourceCycles'].append(1)

        #Add to return-list
        sklWriteResPairs.append(tempDictFolded)

    return sklWriteResPairs

#Find all SKLWriteResGroup defs
def getSklWriteResGroupDefs(sklWriteResGroupDef, schedSkylakeClientTD):
    sklWriteResGroupDefs = []
    for sklWriteResGroup in sklWriteResGroupDef.searchString(schedSkylakeClientTD):
        tempDict = {
                'Name' : sklWriteResGroup['SKLWriteResGroup'],
                'Latency' : int(sklWriteResGroup['Latency']),
                'Resources' : sklWriteResGroup['Resources'].strip(",").strip().split(","),
                'ResourceCycles' : list(map(int, sklWriteResGroup['ResourceCycles'].split(",")))
                }
        sklWriteResGroupDefs.append(tempDict)
    return sklWriteResGroupDefs

#Find all instructions defined for skylake by llvm
def getLlvmInstructions(instrGroupDef, schedSkylakeClientTD):
    instructions = [] 
    for instrGroup in instrGroupDef.searchString(schedSkylakeClientTD):
        instructions.append(instrGroup.asDict())
    return instructions
    #InstrGroup.asDict() returns the following data structure
        # dict = {
                # SKLWriteResGroup
                # instRegex
                # }

#Check if parameter is a number
def isNumber(a):
    try:
        int(a)
        return True
    except ValueError:
        return False

#Find out if there are any instructions not matched to the instructions defined in unison, this function is slow due to testing all combinations of both input lists, so alot of things are aimed at improving its speed
def regexMatching (unisonInstructions, instructions):
    #Dictionary to save results
    matchings = {
            'Matched' :  [],
            'Unmatched' : []
            }

    alNumRegex = []
    notAlNumRegex = []
    #Divide regex'es into those with just alphanumericals, as they dont contain any special rules and we can just perform regular string-matching
    for instruction in instructions:
        #TODO: Allow instruction to contain "_" as well as alphanumericals for speed improvements
        if instruction['Regex'].isalnum():
            alNumRegex.append(instruction)
        else:
            notAlNumRegex.append(instruction)

    tempUnmatched = []
    #See if we get a string match before trying expensive regex
    for unisonInstruction in unisonInstructions:
        # Match possible unison instructions with llvm-regex(containing only alphanumericals) by checking string equality
        match = list(filter(lambda alNum : alNum['Regex'] == unisonInstruction, alNumRegex))
        # Unison-instruction matched a llvm-regex
        if match:
            matching = {
                    'Instruction' : unisonInstruction,
                    'ResourceGroup' : match[0]['ResourceGroup'],
                    }
            matchings['Matched'].append(matching)

        else:
            tempUnmatched.append(unisonInstruction)

    #Perform more expensive regex matching for instructions not found through string-matching
    for instruction in tempUnmatched:
        matched = False
        for regex in notAlNumRegex:
            #Check if we already matched the instruction with an earlier regex
            if matched:
                continue;

            searchResult = re.search(regex['Regex'], instruction) 
            #Check if we matched the whole instruction
            if (not (searchResult is None) and searchResult.end() - searchResult.start() == len(instruction)):
                matching = {
                        'Instruction' : instruction,
                        'ResourceGroup' : regex['ResourceGroup'],
                        }
                matchings['Matched'].append(matching)
                matched = True
        
        if not matched:
            #Instruction wasnt matched with any regex
            matchings['Unmatched'].append({'Instruction' : instruction})

    return matchings

if __name__ == '__main__':
    main()

