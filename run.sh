#!/bin/sh
runghc AntBrain.hs > simple.ant
simulation/Ants -w sample0.world -r ant1.ant -b simple.ant

