#!/bin/bash

echo "net.fortytwo.myotherbrain.revision = `svn info | grep Revision | sed 's/^.*: //'`"
