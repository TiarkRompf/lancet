#usage: python copyrightify <base_dir>
#apply copyright header block to all relevant files

import argparse
import os
from datetime import date

#extensions to apply copyright header block to
default_extensions = [ "scala" ]

year = date.today().year

copyright = "/*\n" + " * Copyright (c) " + str(year) +" Oracle and/or its affiliates. All rights reserved.\n" + """ * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see http://www.gnu.org/licenses/agpl.html.
 * 
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */
"""

def main():
	parser = argparse.ArgumentParser(description='Apply copyright header block to files ending with certain extensions')
	parser.add_argument("base_dir", help="The base directory to start searching from")
	parser.add_argument("--ext", help="File extensions to append header block to", default=default_extensions,action="append")
	args = parser.parse_args()
	#print args.base_dir
	for root, dirs, files in os.walk(args.base_dir):
		for file in files:
			#print "processing file: " + file
			for ext in args.ext:
				#print "checking if file ends with extension ." + ext 
				if file.endswith("."+ext):
					toprocfilename = os.path.join(root, file)
					toprocfile = open(toprocfilename, "r")					
					toprocfilelines = toprocfile.readlines()
					toprocfile.close()
					out = []
					#check if a block already exists
					if len(toprocfilelines) > 2 and toprocfilelines[0].startswith("/*") and toprocfilelines[1].startswith(" * Copyright (c"):
						continue
					else:
						print "Adding copyright block to: " + toprocfilename
						outfile = open(toprocfilename, "w")
						outfile.write(copyright)
						for l in toprocfilelines:
							outfile.write(l)												
						outfile.close()
	
if __name__ == "__main__":
    main()



