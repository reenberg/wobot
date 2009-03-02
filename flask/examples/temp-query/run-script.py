from simcore import *
import simutil, simtime
import re

def load_layout(file):
    layout = {}

    f = open(file, "r")
    positions = f.readlines()
    f.close()

    for i in positions:
        m = re.match(r'(\d+)\s+([\d\.]+)\s+([\d\.]+)', i)
        if m:
            (node, x, y) = m.groups()
            layout[int(node)] = (float(x), float(y))

    return layout

def place_nodes(layout):
    nodes = {}

    for i in layout.keys():
        (x, y) = layout[i]
        motes[i].moveTo(x, y)

    return nodes

def setup(layout_file):
    layout = load_layout(layout_file)
    sim.exec("build/pc/main.exe", len(layout), "-d=sim.log -b=1")
    place_nodes(layout)

def time_stop_handler():
    sim.stop()

#sim.loadPlugin("LocationPlugin");

radio.setCurModel("disc10")
radio.setScalingFactor(0.4)

#radio.setCurModel("empirical")

setup("randgrid_20.mps")

radio.updateModel()
radio.publishModel()

#sim.resume()

#simutil.CallIn(simtime.onemin*10, time_stop_handler);
