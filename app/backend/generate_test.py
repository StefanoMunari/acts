#!/usr/bin/env python
import sys
import os
import json
import subprocess
import shutil

def read_batch(batch_path):
  prefix = "\""
  separator = ","
  postfix = "_harness.adb\""
  with open(batch_path) as json_data:
      test_string = prefix+(postfix+separator+prefix).join(json.load(json_data))+postfix
      test_list = test_string.split(",")
  return (test_string, test_list)

def make_builder_argument(test_list):
  prefix="For Executable ("
  middle=") use "
  postfix="\";"
  for index, val in enumerate(test_list):
    test_list[index]=prefix+test_list[index]+middle+(test_list[index])[:-13]+postfix
  return "\\\n \\\t".join(test_list)

def template_substitution(project_file, data):
  subprocess.call(["sed", "-i", "-e", "s/<MAIN_TEMPLATE>/"+data[0]+"/g", project_file])
  subprocess.call(["sed", "-i", "-e", "s/<BUILDER_TEMPLATE>/"+data[1]+"/g", project_file])
  try:
      os.remove(project_file+"-e")
  except OSError:
      pass

def generate_project(template_file, batch_path):
  project_file = template_file.replace("template", "gpr")
  shutil.copy(template_file, project_file)
  test = read_batch(batch_path)
  template_substitution(project_file, (test[0], make_builder_argument(test[1])))

if(len(sys.argv) == 3):
  generate_project(str(sys.argv[1]), str(sys.argv[2]))
else:
  print "Wrong number of arguments"