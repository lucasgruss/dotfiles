import os
import argparse

parser = argparse.ArgumentParser(description='Program')
parser.add_argument('prog_name', type=str, help='program')
parser.add_argument('cmd', type=str, help='program')
args = parser.parse_args()

def rord(prog_name, cmd):
    """
    Run or Raise or Dismiss.
    """
    open_progs = os.popen("wmctrl -l | cut -f4- -d' '").read().split("\n")
    current_prog = os.popen("xdotool getwindowfocus getwindowname").read()
    if prog_name in current_prog:
        os.system("xdotool windowminimize $(xdotool getactivewindow)")
    else:
        os.system(f"wmctrl -a  {prog_name} || {cmd}")

rord(args.prog_name, args.cmd) 
