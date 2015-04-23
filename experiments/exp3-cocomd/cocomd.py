###
# Parse a (coco)md file to make an html file.
#
# a (coco)md file is md, with slides, user inputs,
# ids, and classes.
###

import re

# get command-line arguments
# read a (coco)md file

def read(filename):
	with open(filename) as f:
		string = f.read()
		f.close()
		return string

shared_cocomd = read("shared.md")
experiment_cocomd = read("exp3-sorites.md")

structure_str, rest = re.split("\n---", experiment_cocomd, 1)
content, include_filenames_str = re.split("\n~~~",
	                                      # put back the beginning, which got
	                                      # cut off in the split:
		                                  "---" + rest,
		                                  1)
shared_content, shared_include_filenames_str = re.split("\n~~~", shared_cocomd)
content = "\n".join([content, shared_content])

structure = re.split("\n+", structure_str.strip())
include_filenames = re.split("\n+", include_filenames_str.strip())
shared_include_filenames = re.split("\n+", shared_include_filenames_str.strip())

def make_include_tag(filename):
	if filename.strip().split(".")[1] == "js":
		return "<script src='" + filename.strip() + "'></script>"
	else:
		return "<link href='" + filename.strip() + "' rel='stylesheet' type='text/css'/>"

head = "\n".join(map(make_include_tag, shared_include_filenames + include_filenames))

replacements = [
    ["//", "<br/>"],
	["!\[(.*)\]\((.*)\).*#(.*)$", "<img alt='\g<1>' src='\g<2>' id='\g<3>'/>"],
	["(\n\n|\A)\s*(.*)$", "\g<1><p>\g<2></p>"],
    ["\n<p>--- *(\w*) *</p>", "\n</div>\n\n<div class='slide' id='\g<1>'>"],
    ["<p>--- *(\w*) *</p>", "<div class='slide' id='\g<1>'>"],
    ["\Z", "\n</div>"],
	["<p>### *(.*)</p>", "<h3>\g<1></h3>"],
	["\{\{([^{}]*)\}\}", "<span id='\g<1>'>{{\g<1>}}</span>"],
	["\[\[\s*(.*)\s*\]\]", "<button type='button' onclick='state.next()'>\g<1></button>"],
	[" \o ", " o "],
	["<p>(.*)#(\w+)\s*</p>", "<p id='\g<2>'>\g<1></p>"]
]

input_matchers = [
	[" ((?:o )+)(.*)?", "radio"],
	["(___+)(.*)?", "text"],
	["(\[textbox.*\])(.*)?", "textbox"],
	["(\[(?:\s*\d+\.\s*.*\n?)+\])(.*)?", "dropdown"]
]

assigned_ids = {}
def assign_id(input_type):
	if input_type in assigned_ids.keys():
		assigned_ids[input_type] += 1
	else:
		assigned_ids[input_type] = 0
	return input_type + str(assigned_ids[input_type])

def get_replacement(matcher, input_type):
	m = re.search(matcher, content)
	if m.group(2):
		text_after = m.group(2)
		id_m = re.search("#(\w+)", text_after)
		if id_m:
			input_id = id_m.group(1)
			text_after = re.sub(id_m.group(0), "", text_after)
		else:
			input_id = assign_id(input_type)
	else:
		text_after = ""
		input_id = assign_id(input_type)
	if input_type == "radio":
		number_of_radios = len(re.split("\s", m.group(1).strip()))

		radios = []
		for i in range(number_of_radios):
			radios.append("<input name='" + input_id +
				          "' type='radio' value='" + str(i) + "'>")
		replacement = " ".join(radios)
	elif input_type == "text":
		replacement = "<input type='text' id='" + input_id + "'></input>"
	elif input_type == "textbox":
		rows, cols = re.search( "\[textbox\s*(\d+)\s*x\s*(\d+)\s*\]",
			                    m.group(1) ).groups()
		replacement = "<textarea id='" + input_id + "' cols='" + cols + \
		              "' rows='" + rows + "'></textarea>"
	elif input_type == "dropdown":
		options = re.split("\d+\.", re.sub("\s+|\[\s*\d+\.\s*|\s*\]",
				                           " ",
				                           m.group(1)))

		replacement = "<select id='" + input_id + "'><label><option value='-1'/></label>"
		for i in range(len(options)):
			replacement += "<label><option value='" + str(i) + "'/>" + options[i] + "</label>"
		replacement += "</select>"
	return replacement + text_after

for matcher, input_type in input_matchers:
	while re.search(matcher, content):
		replacement = get_replacement(matcher, input_type)
		content = re.sub(matcher, replacement, content, 1)

for pattern, repl in replacements:
	content = re.sub(pattern, repl, content, flags=re.M)

progress_bar = """<div class="progress">
    <span>Progress:</span>
    <div class="bar-wrapper">
      <div class="bar" width="0%">
      </div>
    </div>
  </div>"""

content += progress_bar

# print content

with open("exp3-sorites.html", "w") as w:
	w.write("<html><head>" + head + "</head><body>" + content + "</body></html>")
	w.close()