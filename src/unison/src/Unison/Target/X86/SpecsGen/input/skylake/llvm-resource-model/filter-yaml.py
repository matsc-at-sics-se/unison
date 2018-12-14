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
import yaml
import logging
    # logging.debug("splitSchedRW %s", str)

def printf(format, *args):
    sys.stdout.write(format % args)

poisonRegs = (
    'RFP32',
    'RFP64',
    'RFP80',
    'VK1',
    'VK2',
    'VK4',
    'VK8',
    'VK16',
    'VK32',
    'VK64',
    'VK1WM',
    'VK2WM',
    'VK4WM',
    'VK8WM',
    'VK16WM',
    'VK32WM',
    'VK64WM',
    'VR64',
    'FR32X',
    'FR64X',
    'VR128X',
    'VR256X',
    'VR512',
    'BNDR',
    'CONTROL_REG',
    'DEBUG_REG',
    'SEGMENT_REG',
    'RST',
    'FPSW',
    'FP0',
    'ST0'
    )
    

def patchInsn(insn):
    uses = insn['uses']
    operands = insn['operands']
    patched = False
    if len(uses) >= 1 and uses[0] == "off'":
        uses[0] = 'offset'
        insn['uses'] = uses
        patched = True
    elif len(uses) >= 2 and uses[0] == 'src1' and uses[1] == 'src1':
        uses[1] = 'src21'
        uses[2] = 'src22'
        uses[3] = 'src23'
        uses[4] = 'src24'
        uses[5] = 'src25'
        modoperands = []
        modoperands.append(operands[0])
        del operands[0]
        if 'src1' in operands[0]:
            modoperands.append(operands[0])
            del operands[0]
        modoperands.append({uses[1] : ['register', 'use', 'ptr_rc']})
        modoperands.append({uses[2] : operands[0]['src2']})
        del operands[0]
        modoperands.append({uses[3] : operands[0]['src3']})
        del operands[0]
        modoperands.append({uses[4] : operands[0]['src4']})
        del operands[0]
        modoperands.append({uses[5] : operands[0]['src5']})
        del operands[0]
        modoperands.extend(operands)
        operands = modoperands
        patched = True

    if operands != None:
        modoperands = []
        if len(operands) >= 1 and operands[0].keys()[0] == "off'":
            operands[0] = {'offset' : operands[0].values()[0]}
            patched = True
        for operand in operands:
            key = operand.keys()[0]
            value = operand.values()[0]
            if len(modoperands) >= 4 and value == ['register', 'use', 'SEGMENT_REG']:
                operand = {key : 'bound'}
                patched = True
            modoperands.append(operand)
        insn['operands'] = modoperands
    # if patched:
    #     logging.debug("patched %r", insn)
    return insn

def selected(insn):
    affects = insn['affects']
    if affects != None:
        for x in affects:
            if x.keys()[0] in poisonRegs:
                return False
    affectedBy = insn['affected-by']
    if affectedBy != None:
        for x in affectedBy:
            if x.keys()[0] in poisonRegs:
                return False
    operands = insn['operands']
    if operands != None:
        for operand in operands:
            key = operand.keys()[0]
            value = operand.values()[0]
            if type(value) is list and value[2] in poisonRegs:
                return False
    return True

def dump(iset):
    printf("---\n")
    printf("instruction-set:\n")
    printf("\n")
    printf("   - group: allInstructions\n")
    printf("     instructions:\n\n\n")
    for insn in iset['instruction-set'][0]['instructions']:
        printf("        - id:                 %s\n", insn['id'])
        printf("          type:               %s\n", insn['type'])
        printf("          operands:\n")
        if insn['operands'] != None:
            for x in insn['operands']:
                printf("           - %s%s\n", (x.keys()[0]+":").ljust(17), str(x.values()[0]).replace("'",""))
        printf("          uses:               %s\n", str(insn['uses']).replace("'",""))
        printf("          defines:            %s\n", str(insn['defines']).replace("'",""))
        printf("          size:               %d\n", insn['size'])
        printf("          affects:\n")
        if insn['affects'] != None:
            for x in insn['affects']:
                printf("           - %s%s\n", (x.keys()[0]+":").ljust(17), str(x.values()[0]).replace("'",""))
        printf("          affected-by:\n")
        if insn['affected-by'] != None:
            for x in insn['affected-by']:
                printf("           - %s%s\n", (x.keys()[0]+":").ljust(17), str(x.values()[0]).replace("'",""))
        printf("          itinerary:          %s\n", insn['itinerary'])
        printf("\n")
    

def main():
    logging.basicConfig(stream=sys.stderr, level=logging.DEBUG)
    iset = yaml.load(sys.stdin)
    inlist = iset['instruction-set'][0]['instructions']
    outlist = []
    for insn in inlist:
        insn = patchInsn(insn)
        if selected(insn):
            outlist.append(insn)
    iset['instruction-set'][0]['instructions'] = outlist
    # yaml.dump(iset, stream=sys.stdout, indent=8) doesn't do what I want
    dump(iset)

if __name__ == '__main__':
    main()

