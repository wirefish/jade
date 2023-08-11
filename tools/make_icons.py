#!/usr/bin/env python3

import re
import argparse
import math
import os.path
from PIL import Image

description = """
Create a sprite sheet and corresponding CSS classes from a collection of icons.
"""

parser = argparse.ArgumentParser(description=description, conflict_handler="resolve")
parser.add_argument("manifest", type=str, nargs=1, help="manifest file")
parser.add_argument("-o", "--output_dir", metavar="PATH", default=".",
                    help="directory for output files")
parser.add_argument("-b", "--base_name", metavar="BASE", default="icons",
                    help="base name for output files")
parser.add_argument("-s", "--size", default=24, type=int,
                    help="size of each icon sprite")

def read_manifest(s):
    icons = {}
    aliases = {}
    for line in s:
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        elif line.startswith("$"):
            alias, value = line[1:].split(None, 1)
            aliases[alias] = value
        else:
            name, path = line.split(None, 1)
            path = re.sub(r"\$(\w+)", lambda m: aliases[m.group(1)], path)
            icons[name] = path
    return icons

if __name__ == "__main__":
    args = parser.parse_args()
    with open(args.manifest[0]) as f:
        icons = read_manifest(f)

    n = math.ceil(math.sqrt(len(icons)))
    dim = n * args.size

    images = [Image.new("RGBA", (dim, dim)),
              Image.new("RGBA", (dim * 2, dim * 2))]
    offsets = {}

    for i, (name, path) in enumerate(icons.items()):
        x = (i % n) * args.size
        y = (i // n) * args.size
        with Image.open(path) as icon:
            images[0].paste(icon.resize((args.size, args.size)), (x, y))
            images[1].paste(icon.resize((args.size * 2, args.size * 2)), (2 * x, 2 * y))
        offsets[name] = (x, y)

    base = os.path.join(args.output_dir, args.base_name)
    images[0].save(base + ".png", "PNG")
    images[1].save(base + "@2x.png", "PNG")

    with open(base + ".css", "w") as f:
        f.write(f".{args.base_name[:-1]} {{\n")
        f.write(f"  background-image: image-set(url('images/{args.base_name}.png') 1x")
        f.write(f", url('images/{args.base_name}@2x.png') 2x);\n")
        f.write(f"  background-size: {dim}px {dim}px;\n")
        f.write(f"  background-repeat: no-repeat;\n")
        f.write("}\n")
        for name, (x, y) in offsets.items():
            f.write(f".{name} {{ background-position: -{x}px -{y}px; }}\n")
