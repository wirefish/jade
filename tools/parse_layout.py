#!/usr/bin/env python3

import argparse
from collections import defaultdict

description = """
Create a template for a jade region from an ASCII-art representation of its layout.
"""

parser = argparse.ArgumentParser(description=description, conflict_handler="resolve")
parser.add_argument("input", type=str, nargs=1,
                    help="path of input file describing layout")
parser.add_argument("-o", metavar="OUT", default=".", dest="output",
                    help="path of output file")

"""

INPUT FILE FORMAT

The input file consists of several sections, each preceded by a line that begins
with !!. Also, blank lines and lines that begin with ; are ignored.

!! region

The body of this section contains a symbol naming the region on one line, and a
sequence of lines describing its attributes. A blank line ends the region
description. For example:

test_forest
:name "The Forest of Failed Unit Tests"
:climate :temperate

The region name is also used to form the package name in the output.

!! map

The body of this section is grid of characters, where letters and numbers in
alternate rows and columns indicate locations, and links are represented by -,
|, \, /, or X between those rows and columns. A blank line ends the grid. Here's
an example:

F-F-C-C
  | |X|
F-F-C-C

Here we have two types of locations (F and C). North/south connections are | and
east/west connections are -. The X indicates a pair of NW/SE and NE/SW
connections.

!! location

For each location type, this section contains an entry for each location type.
The first line contains the letter for that location followed by a symbol to
name the new prototype and optionally the name of its prototype. This is
followed followed by lines describing its attributes, ending with a blank line.
For example, this section might contain:

F forest
:name "Forest"
:domain :outdoor
:surface :trees

!! portal

This section defines portal prototypes to use when connection locations. For
each pair of location types, this section may contain an entry whose first line
consists of one or more symbols combining two the two location letters, followed
by a name for the new prototype and optionally the name of its prototype. The
following lines define the portal's attributes. A blank line ends the entry. For
example:

FF FC forest-portal continuing-portal
:brief "the forest"

"""

def split_file(f):
    sections = []
    section = None
    block = None
    for line in f:
        line = line.rstrip()
        if line.startswith(";"):
            pass
        elif len(line) == 0:
            if block:
                section.append(block)
                block = None
        elif line.startswith("!!"):
            if section:
                sections.append(section)
            section = [line.split(None, 1)[1]]
        elif section:
            if block:
                block.append(line)
            else:
                block = [line]
    if section:
        if block:
            section.append(block)
        sections.append(section)
    return sections

class Layout:
    def __init__(self):
        self.region = None
        self.location_prototypes = {}
        self.portal_prototypes = {}
        self.map_rows = None
        self.links = {}
        self.reader = None

    def parse_region(self, blocks):
        if len(blocks) != 1:
            print("region section must contain exactly one block")
            return
        self.region = blocks[0]

    def parse_map(self, blocks):
        if len(blocks) != 1:
            print("map section must contain exactly one block")
            return
        block = blocks[0]

        self.cols = 0
        for line in block:
            if len(line) % 2 == 0:
                print(f"length of map line must be odd: \"{line}\"")
                return
            self.cols = max(self.cols, (len(line) + 1) // 2)

        if len(block) % 2 == 0:
            print("map block must contain an odd number of lines")
            return
        self.rows = (len(block) + 1) // 2

        # Add padding to make parsing easier.
        empty_row = " " * (2 * self.cols + 3)
        self.map_rows = [empty_row, empty_row,
                         *["  " + line.ljust(2 * self.cols - 1) for line in block],
                         empty_row, empty_row]

    def parse_locations(self, blocks):
        for block in blocks:
            header, *attrs = block
            parts = header.split()
            key = parts[0]
            name = parts[1]
            if len(parts) == 3:
                proto = parts[2]
            else:
                proto = "location"
            self.location_prototypes[key] = [name, proto] + attrs

    def parse_portals(self, blocks):
        for block in blocks:
            header, *attrs = block
            parts = header.split()
            num_keys = next(i for i, item in enumerate(parts) if len(item) != 2)
            name = parts[num_keys]
            if len(parts) > num_keys + 1:
                proto = parts[num_keys + 1]
            else:
                proto = ""
            for key in parts[:num_keys]:
                key = "".join(sorted(key))
                self.portal_prototypes[key] = [name, proto] + attrs

    def parse(self, f):
        for section in split_file(f):
            name, *blocks = section
            if name == "region":
                self.parse_region(blocks)
            elif name == "map":
                self.parse_map(blocks)
            elif name == "location":
                self.parse_locations(blocks)
            elif name == "portal":
                self.parse_portals(blocks)
            else:
                print(f"ignoring invalid region {name}")

    def write_attributes(self, attrs, f):
        f.write(f"  ({attrs[0]}")
        for attr in attrs[1:]:
            f.write(f"\n   {attr}")
        f.write(")")

    def write_definition(self, prefix, attrs, f):
        f.write(f"({prefix}\n")
        self.write_attributes(attrs, f)
        f.write(")\n\n")

    def write_region(self, f):
        name, *attrs = self.region
        f.write(f"(in-package :jade.{name})\n\n")
        self.write_definition(f"defentity {name} ()", attrs, f)

    def write_prototypes(self, blocks, f):
        for block in blocks:
            name, proto, *attrs = block
            self.write_definition(f"defentity {name} ({proto})", attrs, f)

    def write_portal_prototypes(self, f):
        unique = {}
        for proto in self.portal_prototypes.values():
            unique[proto[0]] = proto
        f.write(";;; portal prototypes\n\n")
        self.write_prototypes(unique.values(), f)

    def col_label(self, i):
        if i < 26:
            return chr(ord('A') + i)
        else:
            return chr(ord('a') + (i - 26))

    def location_suffix(self, i, j):
        return f"{self.col_label(i)}{j:02}"

    def write_map(self, f):
        f.write("#|\n")
        f.write("    " + " ".join([self.col_label(i) for i in range(0, self.cols)]) + "\n")
        for j, row in enumerate(self.map_rows[1:-1]):
            if (j % 2):
                f.write(f"{j // 2:02}")
            else:
                f.write("  ")
            f.write(row.rstrip() + "\n")
        f.write("|#\n\n")

    def location_label(self, letter, i, j):
        return f"{self.location_prototypes[letter][0]}-{self.location_suffix(i, j)}"

    exit_directions = [
        [-1, -1, ":northwest"],
        [-1,  0, ":west"],
        [-1,  1, ":southwest"],
        [ 0, -1, ":north"],
        [ 0,  1, ":south"],
        [ 1, -1, ":northeast"],
        [ 1,  0, ":east"],
        [ 1,  1, ":southeast"]
    ]

    def get_exits(self, letter, i, j):
        x = 2 + 2 * i
        y = 2 + 2 * j
        rows = self.map_rows
        exits = defaultdict(list)
        for (dx, dy, dir_name) in Layout.exit_directions:
            if dx or dy:
                link = rows[y + dy][x + dx]
                if link != " ":
                    dest_letter = rows[y + dy * 2][x + dx * 2]
                    dest = self.location_label(dest_letter, i + dx, j + dy)
                    portal_key = "".join(sorted([letter, dest_letter]))
                    portal = self.portal_prototypes[portal_key][0]
                    exits[portal].append([dir_name, dest])
        return exits

    def format_exits(self, exits):
        parts = []
        for (portal, instances) in exits.items():
            s = f"({portal}"
            for (dir, dest) in instances:
                s += f" {dir} {dest}"
            s += ")"
            parts.append(s)
        sep = "\n" + " " * 11
        return f":exits ({sep.join(parts)})"

    def write_location(self, letter, i, j):
        exits = self.get_exits(letter, i, j)
        label = self.location_label(letter, i, j)
        proto = self.location_prototypes[letter][0]
        self.write_definition(f"deflocation {label} ({proto})",
                              [self.format_exits(exits)], f)

    def write_locations(self, f):
        groups = defaultdict(list)
        rows = self.map_rows
        for j in range(0, (len(rows) - 3) // 2):
            y = 2 + j * 2
            row = rows[2 + j * 2]
            for i in range(0, (len(row) - 3) // 2):
                letter = row[2 + i * 2]
                if letter != " ":
                    groups[letter].append([i, j])

        for (letter, ijs) in groups.items():
            f.write(f";;; {self.location_prototypes[letter][0]}\n\n")
            self.write_prototypes([self.location_prototypes[letter]], f)
            for (i, j) in ijs:
                self.write_location(letter, i, j)

    def save(self, f):
        self.write_region(f)
        self.write_map(f)
        self.write_portal_prototypes(f)
        self.write_locations(f)
        print(f"map is {self.cols} by {self.rows}")

if __name__ == "__main__":
    args = parser.parse_args()

    layout = Layout()
    with open(args.input[0]) as f:
        layout.parse(f)

    with open(args.output, "w") as f:
        layout.save(f)
