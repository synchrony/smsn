// The origin is at the "bottom right" of the case (in the empty space where the lid goes),
// with the thumb to the north (the y axis), the fingers of the left hand to the left
// (the x axis) and pointing away, and the bottom of the case pointing down (the z axis)
// away from the palm.

function pythag(a, b) = sqrt(a*a + b*b);
function angle(a, b) = atan(a/b);

// toggle this variable to see the ICs in position and make sure they fit inside the case
visualizeInternalComponents = true;

// this is the accuracy value for Shapeways' "Strong & Flexible Plastics" option
error = 0.15;

error2 = error * 2;

// note: AFAIK, "clearance" is a minimum gap in printed parts.  Here, however, it is used
// as a lesser/safer accuracy for gaps between the printed part and foreign objects such as
// circuit boards, which may not have been cut or measured with the same degree of precision.
clearance = 0.5;

pinHoleRes = 10;
/*
cornerRoundingRes = 100;
connectorPinRes = 100;
ledHoleRes = 100;
//*/
//*
cornerRoundingRes = 10;
connectorPinRes = 10;
ledHoleRes = 10;
//*/
containmentWallThick = 1;

thumbCurveRadius = 39.0;
offsetFromTopEdgeToFirstFingerWell = 15.0;
fingerWidth = 84 / 4;  // 21 -- 82mm was measured, but this seemed a little cramped
thumbWidth = 25;
horizontalOffsetBeforeThumbWell = 25.0;
buttonBodySeparation = 1.5;
thumbButtonTiltDegrees = 5;

nanoLength = 43.2;
nanoWidth = 18.0;
nanoHeightWithoutUsb = 5.4;
nanoHeightToUsbBase = 3.3;
nanoUsbWidth = 7.6;
nanoUsbHeight = 3.9;

chargerHeaderLength = 5.1;
chargerHeaderWidth = 2.6;

powerSwitchLength = 11.6;
powerSwitchWidth = 4.0;
powerSwitchDepth = 7.5;

// horizontal offset of components with ports/controls on the bottom edge
nanoOffset = 0;
nanoOuterWallOffset = nanoOffset + nanoWidth + 2*clearance;
chargerHeaderOffset = nanoOuterWallOffset + containmentWallThick;
powerSwitchOffset = chargerHeaderOffset + chargerHeaderWidth + error2;

ledDomeRadius = 2.5;
ledRimRadius = 2.95;
ledRimThick = 1.0;

lidThick = 1.5;
floorThick = 1.5;

caseWidth = 70;
caseLength = 130;
thickestComponent = 7.9;
caseHeight = lidThick + floorThick + thickestComponent + 1;

wallThick = caseHeight/2;
caseCornerRadius = wallThick;  // TODO: give the case a square profile by making caseCornerRadius less than half of the case height

echo("caseHeight: ", caseHeight);
echo("wallThick: ", wallThick);

pushButtonWellWidth = 7;
pushButtonHeight = 5.0;
pushButtonLegLength = 3.4;
pushButtonPressDepth = 0.25;
pushButtonWellDepth = 4.0;
pushButtonBaseThick = 1.5;

fingerWellDepth = 5;
thumbWellDepth = 5;
buttonThick = 8;
buttonWidth = pushButtonWellWidth;
fingerButtonLength = fingerWidth - buttonBodySeparation;
thumbButtonLength = thumbWidth - buttonBodySeparation;
buttonStabilizerWellWidth = 3;
buttonClearance = 0.3;  // horizontal clearance between button and walls
buttonLip = 2.5;
buttonSlack = 0.5;  // minimum vertical clearance between button and floor/retainers
buttonRetainerThick = 1.0;
buttonStabilizerRodHeight = pushButtonHeight - (pushButtonPressDepth + buttonSlack + error2);

// buttons are inset by only 1/3 of the depth of the finger or thumb well
totalFingerWellDepth = fingerWellDepth/3 + buttonThick + pushButtonHeight;
totalThumbWellDepth = thumbWellDepth/3 + buttonThick + pushButtonHeight;

pinHoleRadius = 0.8;

thumbBevelLength = 48;
thumbBevelWidth = 31;
thumbBevelBuffer = 2;

thumbBevelAngle = angle(thumbBevelLength-wallThick,thumbBevelWidth-wallThick);
thumbBevelStretch = pythag(thumbBevelLength-wallThick, thumbBevelWidth-wallThick);

cavityWidth = caseWidth - wallThick - totalFingerWellDepth - pushButtonBaseThick;
cavityLength = caseLength - (2 * wallThick);

// for visualization only
nanoVizOffsetX = 1;
nanoVizOffsetY = 1;
modemVizOffsetX = nanoOffset + nanoWidth + containmentWallThick + 1;
modemVizOffsetY = 1;
motionSensorVizOffsetX = 1;
motionSensorVizOffsetY = nanoLength + containmentWallThick + 1;
batteryVizOffsetX = modemVizOffsetX;
batteryVizOffsetY = motionSensorVizOffsetY;
transducerVizOffsetX = cavityWidth - 15;
transducerVizOffsetY = batteryVizOffsetY + 47 + 1;
        
// creates a button of the given length.  Clearance on all sides of the button is subtracted.
module button(length) {
    difference() {
        union() {
            // rounded cap
            translate([buttonClearance,buttonWidth/2,buttonWidth/2]) {
                rotate([0,90,0]) { cylinder(h=length-2*buttonClearance,r=buttonWidth/2-buttonClearance, $fn=cornerRoundingRes); }
            }

            // button body
            translate([buttonClearance,buttonClearance,buttonWidth/2]) {
                cube([length-2*buttonClearance,buttonWidth-2*buttonClearance,buttonThick-buttonWidth/2]);
            }

            // stabilizer rods
            translate([buttonClearance,buttonClearance,buttonThick]) {
                cube([buttonStabilizerWellWidth-2*buttonClearance, buttonWidth-2*buttonClearance, buttonStabilizerRodHeight]);
            }
            translate([length-buttonStabilizerWellWidth+buttonClearance,buttonClearance,buttonThick]) {
                cube([buttonStabilizerWellWidth-2*buttonClearance, buttonWidth-2*buttonClearance, buttonStabilizerRodHeight]);
            }
            // retainer rods
            translate([buttonClearance,buttonClearance,buttonThick+pushButtonHeight-pushButtonWellDepth+buttonRetainerThick+buttonSlack+error2]) {
                cube([buttonStabilizerWellWidth+buttonLip-2*buttonClearance,buttonWidth-2*buttonClearance, 1]);
            }
            translate([length-buttonStabilizerWellWidth-buttonLip+buttonClearance,buttonClearance,buttonThick+pushButtonHeight-pushButtonWellDepth+buttonRetainerThick+buttonSlack+error2]) {
                cube([buttonStabilizerWellWidth+buttonLip-2*buttonClearance,buttonWidth-2*buttonClearance, 1]);
            }
        }

	    // hollow out the button to save plastic/money
        translate([0,buttonWidth/2,buttonWidth/2]) {
            rotate([0,90,0]) { cylinder(h=length+1,r=buttonWidth/3); }
        }
    }
}

// button well with origin at the center of the base of the switch
// legs of the switch run along the x axis
module pushButtonWell(length, depth) {
    translate([-pushButtonWellWidth/2, -pushButtonWellWidth/2, 0]) {

		// square well for body of switch
        cube([pushButtonWellWidth,pushButtonWellWidth, depth]);

		// channels for legs/wires of switch
        translate([0,0,-pushButtonLegLength]) {
                translate([0,0,0]) {
                    cube([1.5,1.5,depth+pushButtonLegLength]);
                }
                translate([pushButtonWellWidth-1.5,0, 0]) {
                    cube([1.5,1.5,depth+pushButtonLegLength]);
                } 
                translate([0,pushButtonWellWidth-1.5, 0]) {
                    cube([1.5,1.5,depth+pushButtonLegLength]);
                } 
                translate([pushButtonWellWidth-1.5,pushButtonWellWidth-1.5, 0]) {
                    cube([1.5,1.5,depth+pushButtonLegLength]);
                }
        }		
    }

    // rectangular well for button body, and stabilizer wells
    translate([-length/2,-buttonWidth/2,pushButtonWellDepth]) {
        cube([length,buttonWidth,buttonThick+10]);

        translate([0,0,-pushButtonWellDepth]) {
            cube([buttonStabilizerWellWidth, buttonWidth, pushButtonWellDepth]);

            cube([buttonStabilizerWellWidth+buttonLip, buttonWidth, pushButtonWellDepth-buttonRetainerThick]);
		}

        translate([length-buttonStabilizerWellWidth,0,-pushButtonWellDepth]) {
            cube([buttonStabilizerWellWidth, buttonWidth, pushButtonWellDepth]);

            translate([-buttonLip,0,0]) {
                cube([buttonStabilizerWellWidth+buttonLip, buttonWidth, pushButtonWellDepth-buttonRetainerThick]);
            }
		}
    }
}

// depth: depth of circular well for the finger
// width: width of the entire construction
// totalWellDepth: depth to base of push button switch
// buttonLength: length of the moving part
module fingerWell(depth, width, totalWellDepth, buttonLength) {
    x = width / 2;
    y = depth;
    radius = (x*x + y*y) / (2*y);

    translate([radius - depth,0,0]) {
        cylinder(h=caseHeight,r=radius, $fn=cornerRoundingRes);
    }

    translate([radius - depth, 0, wallThick]) {
        cylinder(h=wallThick+.001, r1=radius-wallThick/2, r2=radius+wallThick/2, $fn=cornerRoundingRes);    
    }
 
    translate([radius - depth,0,0]) {
        cylinder(h=wallThick+.001, r2=radius-wallThick/2, r1=radius+wallThick/2, $fn=cornerRoundingRes);    
    }

    // well and channels for push button switch and wires
    translate([-totalWellDepth,0,caseHeight/2]) {
        rotate([0,90,0]) { rotate([0,0,90]) {
            pushButtonWell(buttonLength, pushButtonWellDepth);
        }}
    }
}

module fingerContainer(totalWellDepth, buttonLength) {
//    translate([-totalWellDepth-pushButtonBaseThick, -buttonLength-buttonBodySeparation, lidThick]) {
//    translate([-totalWellDepth-pushButtonBaseThick, 0, lidThick]) {
    translate([-totalWellDepth-pushButtonBaseThick, -buttonLength/2-buttonBodySeparation, lidThick]) {
        cube([totalWellDepth+pushButtonBaseThick, buttonLength + 2*buttonBodySeparation, caseHeight-lidThick]);
    }
}

// for the thumb rest
module roundedCylinder(height, radius, cornerRadius) {
    cylinder(r=(thumbCurveRadius - caseCornerRadius), h=caseHeight);
        translate([0, 0, caseCornerRadius]) {
            rotate_extrude(convexity = 10) {
                translate([thumbCurveRadius - caseCornerRadius, 0, 0]) {
                    circle(r = caseCornerRadius);
                }
            }
        cylinder(r=thumbCurveRadius, h=(caseHeight - 2*caseCornerRadius));
    }
    translate([0, 0, caseHeight - caseCornerRadius]) {
        rotate_extrude(convexity = 10) {
            translate([thumbCurveRadius - caseCornerRadius, 0, 0]) {
                circle(r = caseCornerRadius);
            }
        }
    } 
}

// for the rounded flat edges of the case
module roundedEdge(length, height, cornerRadius) {
    rem = height - 2*cornerRadius;
    intersection() {
        translate([-height/2, 0, 0]) {
            cube([height, cornerRadius, length]);
        }
        union() {
            translate([-rem/2.0, 0, 0]) {
                cylinder(h=length, r=cornerRadius);
            }
            translate([rem/2.0, 0, 0]) {
                cylinder(h=length, r=cornerRadius);
            }
            translate([-rem/2.0, -cornerRadius, 0]) {
                cube([rem, 2 * cornerRadius, length]);
            }
        }
    }
}

// for the intersections between rounded edges
module roundedCorner(height, cornerRadius) {
    difference() {
        union() {
            translate([0, 0, -height/2.0 + cornerRadius]) {
                sphere(r=cornerRadius);
                cylinder(r=cornerRadius, h=(height - 2*cornerRadius));
            }
            translate([0, 0, height/2.0 - cornerRadius]) {
                sphere(r=cornerRadius);
            } 
        }
        translate([0, 0, -height/2]) {
            cube([cornerRadius, cornerRadius, height]);
        }
    }  
}

module pinHole() {
    cylinder(h=caseHeight+2,r=pinHoleRadius, $fn=pinHoleRes);
}

module pinHoles() {
    translate([0,0,-1]) {
        for (i = [1:4]) {
            translate([wallThick*2/3,wallThick*2/3 + i*(caseLength-thumbCurveRadius - wallThick*2/3)/4,0]) {
                pinHole();
            }
        }
        // pinhole on right edge close to the lower right corner
        translate([wallThick*2/3,10,0]) {
            pinHole();
        }

        // pinholes on the bottom edge which avoid interfering with the battery charger port, power switch, and USB port
        translate([10,wallThick*2/3,0]) {
            pinHole();
        }
        translate([wallThick+cavityWidth-nanoLeftMargin-nanoWidth/2-8,wallThick*2/3,0]) {
            pinHole();
        }
        translate([wallThick+cavityWidth-nanoLeftMargin-nanoWidth/2+8,wallThick*2/3,0]) {
            pinHole();
        }
    }
}

module caseConvexHull() {
    // box with cutout for thumb cylinder
    difference() {
        translate([wallThick, wallThick,0]) {
            cube([caseWidth-caseHeight,caseLength-caseHeight,caseHeight]);
        }
        translate([0, caseLength - thumbCurveRadius, -1]) {
            cube([thumbCurveRadius, thumbCurveRadius, caseHeight + 2]);
        }
    }

    // thumb rest
    difference() {
        translate([thumbCurveRadius, caseLength - thumbCurveRadius, 0]) {
            intersection() {
                 roundedCylinder(caseHeight, thumbCurveRadius, caseCornerRadius);
                 translate([-thumbCurveRadius, 0, 0]) {
                    cube([thumbCurveRadius, thumbCurveRadius, caseHeight]);
                }
            }
        }
        translate([thumbCurveRadius, caseLength - thumbCurveRadius, lidThick]) {
            intersection() {
                cylinder(r=(thumbCurveRadius - caseCornerRadius), h=(caseHeight - lidThick - floorThick));
                translate([-thumbCurveRadius, 0, 0]) { cube([thumbCurveRadius, thumbCurveRadius, caseHeight]); }
            }
        }
    }

    // rounded edges and corners
    ccr = caseCornerRadius;
    shortEdge = caseWidth - 2*ccr;
    mediumEdge = caseLength - thumbCurveRadius - ccr;
    longEdge = caseLength - 2 * ccr;
    tinyEdge = caseWidth - thumbCurveRadius - ccr;
    translate([ccr, ccr, caseHeight/2.0]) {
        roundedCorner(caseHeight, ccr);
        rotate([-90, 0, 0]) { rotate([0, 0, 90]) {
            roundedEdge(mediumEdge, caseHeight, ccr);
        }}
        rotate([0, 90, 0]) { rotate([0, 0, 180]) {
            roundedEdge(shortEdge, caseHeight, ccr);
        }}
    }
    translate([caseWidth-ccr, ccr, caseHeight/2.0]) {
        rotate([0, 0, 90]) { roundedCorner(caseHeight, ccr); }
        rotate([-90, 0, 0]) { rotate([0, 0, -90]) {
            roundedEdge(longEdge, caseHeight, ccr);
        }}
    }
    translate([caseWidth-ccr, caseLength-ccr, caseHeight/2.0]) {
        rotate([0, 0, 180]) { roundedCorner(caseHeight, ccr); }
        rotate([0, -90, 0]) {
            roundedEdge(tinyEdge, caseHeight, ccr);
        }
    }
}

thumbWellDepression = 2;

module thumbButtonContainer() {
    r = thumbCurveRadius;
    x = r - horizontalOffsetBeforeThumbWell;
    y = sqrt(r*r - x*x);
    translate([horizontalOffsetBeforeThumbWell, caseLength - r + y - thumbWellDepression, 0]) {
        rotate([0,0,90]) {
            translate([0,-(thumbButtonLength+buttonBodySeparation)/2],0){
                fingerContainer(totalFingerWellDepth, thumbButtonLength);
            }
        }
    }
}

module thumbButtonWell() {
    r = thumbCurveRadius;
    x = r - horizontalOffsetBeforeThumbWell;
    y = sqrt(r*r - x*x);
    translate([horizontalOffsetBeforeThumbWell, caseLength - r + y - thumbWellDepression, 0]) {
        rotate([0,0,90]) {
            translate([0,-(thumbButtonLength+buttonBodySeparation)/2],0){
                fingerWell(thumbWellDepth, thumbWidth, totalFingerWellDepth, thumbButtonLength);
            }
        }
    }
}

module basicCase() {
  difference() {
    union() {
        difference() {
            caseConvexHull();

            // inner compartment
            translate([wallThick,wallThick,lidThick]) {
                cube([cavityWidth,caseLength-thumbCurveRadius-wallThick+0.001,caseHeight-floorThick-lidThick]);
            }
            translate([thumbCurveRadius,wallThick,lidThick]) {
                cube([cavityWidth+wallThick-thumbCurveRadius,cavityLength,caseHeight-floorThick-lidThick]);
            }
        }

        thumbButtonContainer();
    }

    pinHoles();

    // finger button wells
    for (i = [0:3]) {
        translate([
            caseWidth,
            caseLength - offsetFromTopEdgeToFirstFingerWell - fingerWidth * (i + 0.5),
            0]) {
                fingerWell(fingerWellDepth, fingerWidth, totalFingerWellDepth, fingerButtonLength);
        }      
    }

    thumbButtonWell();

    // hole for status LED
    translate([0,40,caseHeight/2]) {
    	    rotate(a=[0,90,0]) {
            cylinder(h=wallThick,r=ledDomeRadius+error, $fn=ledHoleRes);

            // note: the rim depression uses a larger error/clearance, since it doesn't need
            // to fit tightly and can't be filed down
            translate([0,0,-2+wallThick]) {
//            translate([0,0,-1+wallThick+ledRimThick]) {
                cylinder(h=10,r=ledRimRadius+clearance, $fn=20);
            }
        }  
    }

    // holes for ports/controls on bottom edge
    translate([wallThick, 0, 0]) {
        // hole for Arduino USB port
        translate([nanoOffset+clearance+nanoWidth/2-nanoUsbWidth/2,0,caseHeight-floorThick-nanoHeightToUsbBase-nanoUsbHeight-clearance]) {
            cube([nanoUsbWidth+2*clearance,wallThick+1,nanoUsbHeight+clearance]);
        }

        // hole for battery charger headers
        translate([chargerHeaderOffset,0,(caseHeight-chargerHeaderLength-error2)/2]) {
            cube([chargerHeaderWidth+error2,wallThick+1,chargerHeaderLength+error2]);
        }

        // hole for SPDT Mini Power Switch
        translate([powerSwitchOffset+containmentWallThick,0,wallThick-powerSwitchWidth/2-error]) {
            cube([powerSwitchLength+error2,wallThick+1,powerSwitchWidth+error2]);
        }
    }
  }
}

// body
difference() {
    basicCase();

    cube([caseWidth, caseLength, (caseHeight-buttonWidth)/2+0.0001]);

    // matching holes for alignment pegs
    translate([caseWidth-wallThick, wallThick, 0]) {
        cylinder(h=caseHeight*2/3, r=1.5+error, $fn=connectorPinRes);
    }
    translate([caseWidth-wallThick, caseLength-wallThick, 0]) {
        cylinder(h=caseHeight*2/3, r=1.5+error, $fn=connectorPinRes);
    }

    // screwdriver/leverage slots
    translate([wallThick,wallThick/2,(caseHeight-buttonWidth)/2]) {
        rotate([150,0,0]) { cube([5,10,10]); }
    }
    translate([wallThick+cavityWidth+3,wallThick/2,(caseHeight-buttonWidth)/2]) {
        rotate([150,0,0]) { cube([5,10,10]); }
    }
    translate([wallThick+cavityWidth+3,caseLength-wallThick/2,(caseHeight-buttonWidth)/2]) {
        rotate([-60,0,0]) { cube([5,10,10]); }
    }
}

// lid
translate([caseWidth + 10, 0, 0]) {
    difference() {
        basicCase();
        translate([0,0,(caseHeight-buttonWidth)/2]) {
            cube([caseWidth, caseLength, caseHeight]);
        }
    }

    // alignment pegs
    translate([caseWidth-wallThick, wallThick, lidThick]) {
        cylinder(h=caseHeight/3, r=1.5, $fn=connectorPinRes);
    }
    translate([caseWidth-wallThick, caseLength-wallThick, lidThick]) {
        cylinder(h=caseHeight/3, r=1.5, $fn=connectorPinRes);
    }
}


//tmp
/*
    r = thumbCurveRadius;
    x = r - horizontalOffsetBeforeThumbWell;
    y = sqrt(r*r - x*x);
    translate([horizontalOffsetBeforeThumbWell, caseLength - r + y, 0]) {
        cube([10,20,40]);
    }
*/

// buttons
// Note: the buttons are oriented so as to make the longest surfaces the smoothest
translate([-10,0,10]) {
    for (i = [0:3]) {
        translate([0,i*(fingerButtonLength+2),0]) { rotate([90,180,-90]) {
            button(fingerButtonLength);
        }}     
    }
}
translate([-2,caseLength-35,10]) {
    rotate([90,180,-90]) {
        button(thumbButtonLength);
    } 
}

// electronic components
translate([wallThick,wallThick,0]) {

    // container for Arduino Nano v3.0
    translate([nanoOffset,0,0]) {
        translate([0,0,caseHeight-floorThick-nanoHeightToUsbBase-error]) {
            translate([nanoWidth+2*clearance,0,0]) {
                cube([containmentWallThick,nanoLength+2*clearance+containmentWallThick,nanoHeightToUsbBase+error]);
            }
            translate([0,nanoLength+2*clearance,0]) {
                cube([nanoWidth+2*clearance+containmentWallThick,containmentWallThick,nanoHeightToUsbBase+error]);
            }
        }
    }

    // container for power switch
    translate([powerSwitchOffset,0,wallThick-powerSwitchWidth/2-error]) {
        cube([containmentWallThick,powerSwitchDepth-wallThick+error+containmentWallThick,caseHeight-floorThick-wallThick+powerSwitchWidth/2+error]);
        translate([containmentWallThick+powerSwitchLength+error2,0,0]) {
            cube([containmentWallThick,powerSwitchDepth-wallThick+error+containmentWallThick,caseHeight-floorThick-wallThick+powerSwitchWidth/2+error]);
        }
        translate([0,0,powerSwitchWidth+2*error]) {
            cube([powerSwitchLength+2*(containmentWallThick+error),powerSwitchDepth-wallThick+error+containmentWallThick,caseHeight-floorThick-wallThick-powerSwitchWidth/2-error]);
        }
        translate([0,powerSwitchDepth-wallThick+error,powerSwitchWidth+2*error-1]) {
            cube([powerSwitchLength+2*(containmentWallThick+error),containmentWallThick,1]);
        }
    }

    if (visualizeInternalComponents) {
        // Arduino Nano v3.0
        translate([nanoVizOffsetX,nanoVizOffsetY,-10]) {
            cube([nanoWidth,nanoLength,nanoHeightWithoutUsb]);
        }

        // Surface Transducer - Small
        translate([transducerVizOffsetX,transducerVizOffsetY,-10]) {
           cube([13.8,21.4,7.9]);
        }
        // Bluetooth Modem - BlueSMiRF Silver -- 42.0 x 16.0 x 3.9
        translate([modemVizOffsetX,modemVizOffsetY,-10]) { cube([16,42,3.9]); }
        // Polymer Lithium Ion Battery - 110mAh
        //translate([-40,0,-10]) { cube([12,28,5.7]); }
        // Polymer Lithium Ion Battery - 400mAh
	    translate([batteryVizOffsetX,batteryVizOffsetY,-10]) { cube([25,35,5]); }
        // Triple Axis Accelerometer & Gyro Breakout - MPU-6050
        translate([motionSensorVizOffsetX,motionSensorVizOffsetY,-10]) { cube([15.5, 25.7, 2.5]); }
    }
}
