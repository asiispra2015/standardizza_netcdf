#!/bin/bash

#b -64 ( o b - 32) serve per aumentare la precisione quando ci sono i messaggi di warning da parte di cdo.  32 o 64: stesso risultato

cdo -b 64 yearavg ${1} ${1%.nc}_media.nc
cdo -b 64 yearstd ${1} ${1%.nc}_std.nc

cdo -b 64 sub ${1} ${1%.nc}_media.nc ${1%.nc}_anomalia.nc
cdo -b 64 div ${1%.nc}_anomalia.nc ${1%.nc}_std.nc ${1%.nc}.s.nc

rm -rf ${1%.nc}_media.nc ${1%.nc}_std.nc ${1%.nc}_anomalia.nc
