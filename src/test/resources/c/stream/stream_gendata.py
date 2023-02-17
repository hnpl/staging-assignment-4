#!/usr/bin/env python3

# Copyright (c) 2023 The Regents of the University of California
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met: redistributions of source code must retain the above copyright  
# notice, this list of conditions and the following disclaimer;
# redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution;
# neither the name of the copyright holders nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import argparse
from collections import deque
import random
import sys
import math

class Options:
    def __init__(self, seed, array_size, stride, data_type):
        self.seed = seed
        self.array_size = array_size
        self.stride = stride
        self.data_type = data_type

class CFileGenerator:
    def __init__(self, filename):
        self.filename = filename
        self.lines = deque()
    def prepend_line(self, line):
        self.lines.appendleft(line)
    def append_line(self, line):
        self.lines.append(line)
    def gen_define(self, symbol, val=""):
        if not val:
            return " ".join(["#define", symbol])
        return " ".join(["#define", symbol, val])
    def gen_header_guard(self, name):
        return [" ".join(["#ifndef", name]),
                self.gen_define(name),
                " ".join(["#endif //", name])
               ]
    def gen_typedef(self, _type, name):
        return " ".join(["typedef", _type, name, ";"])
    def gen_static_array(self, _type, name, size, elements, num_elements_per_line=16):
        lines = []
        lines.append(f"static {_type} {name}[{size}] =")
        lines.append(f"{{")
        for idx_line in range((len(elements) + num_elements_per_line - 1)//num_elements_per_line):
            curr_line = []
            for idx_element in range(idx_line * num_elements_per_line, (idx_line+1)*num_elements_per_line):
                curr_line.append(f"{elements[idx_element]:>8},")
            lines.append("".join(curr_line))
        lines.append(f"}};")
        return lines
    def gen_comment(self, comment):
        return f"// {comment}"
    def gen_empty_line(self):
        return ""
    def add_define(self, symbol, val=""):
        self.append_line(self.gen_define(symbol, val))
    def add_typedef(self, _type, name):
        self.append_line(self.gen_typedef(_type, name))
    def add_static_array(self, _type, name, size, elements, num_elements_per_line=16):
        for line in self.gen_static_array(_type, name, size, elements, num_elements_per_line):
            self.append_line(line)
    def add_empty_line(self):
        self.append_line(self.gen_empty_line())
    def add_header_guard(self, name):
        guard = self.gen_header_guard(name)
        self.prepend_line(guard[1])
        self.prepend_line(guard[0])
        self.append_line(guard[2])
    def add_comment(self, comment):
        self.append_line(self.gen_comment(comment))
    def emit_file(self):
        with open(self.filename, "w") as f:
            f.write("\n".join(self.lines))

class MatrixGenerator:
    def __init__(self, seed):
        random.seed(seed)
    def generate_elements(self, n_elements, element_range):
        min_element, max_element = element_range
        ans = [random.randint(min_element, max_element) for _ in range(n_elements)]
        return ans

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--num-accessed-elements", required=True, type=int,
                        help="Number of elements to be accessed.")
    parser.add_argument("--stride", required=True, type=int,
                        help="Distance of elements of consecutive accesses")
    parser.add_argument("--output-name", required=True, type=str,
                        help="Name of the output file.")
    parser.add_argument("--data-type", required=True, type=str,
                        help="Data type of each element in the matrices, e.g. int, long.")

    args = parser.parse_args()

    options = Options(seed = 154,
                      array_size = args.num_accessed_elements * args.stride,
                      stride = args.stride,
                      data_type = args.data_type)

    matrixgenerator = MatrixGenerator(seed = options.seed)
    source = matrixgenerator.generate_elements(options.array_size, element_range = (0, 2**15-1))
    verify = source

    cfilegenerator = CFileGenerator(args.output_name)
    cfilegenerator.add_comment("Command: {}".format(" ".join(sys.argv)))
    cfilegenerator.add_define("STRIDE", str(options.stride))
    cfilegenerator.add_define("ARRAY_SIZE", str(options.array_size))
    verify_function_name = {"int": "verifyWithStride", "short": "verifyShortWithStride", "long": "verifyLongWithStride"}[options.data_type]
    cfilegenerator.add_define("VERIFY_FUNCTION", verify_function_name)
    cfilegenerator.add_typedef(options.data_type, "data_t")
    cfilegenerator.add_static_array("data_t", "source_data", options.array_size, list(map(str, source)), 8)
    cfilegenerator.add_static_array("data_t", "verify_data", options.array_size, list(map(str, verify)), 8)
    cfilegenerator.add_header_guard("__DATASET_H")
    cfilegenerator.emit_file()
