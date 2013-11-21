/*
OpenSCAD design for Typeatron Mark 1
Copyright 2013 by Joshua Shinavier

Note: the origin is at the "bottom right" of the case (in the empty space where the lid goes),
with the thumb to the north (the y axis), the fingers of the left hand to the left
(the x axis) and pointing away, and the bottom of the case pointing down (the z axis)
away from the palm.

Some details to double-check before printing:
  * channel for index finger's pressure sensor does not intersect thumb button structure
    Ideally, let it be entirely below, so that the sensor can lie flat on the case floor

*/

function sq(x) = x*x;


////////////////////////////////////////////////////////////////////////////////
// rendering settings

// toggle this variable to see the ICs in position and make sure they fit inside the case
visualizeInternalComponents = false;
buttonsInSitu = false;

simpleEdges = true;

/*
cornerRoundingRes = 100;
ledDomeRes = 100;
thumbCurveRes = 50;
//*/
//*
cornerRoundingRes = 10;
ledDomeRes = 10;
thumbCurveRes = 10;
//*/

ledRimRes = 20;
lightSensorWellRes = 20;
pinHoleRes = 10;


////////////////////////////////////////////////////////////////////////////////
// printer parameters

// the following values are for Shapeways' "Strong & Flexible Plastics" material
shapewaysAccuracy = 0.15;  // then 0.15% of longest axis

//accuracy = shapewaysAccuracy;
//accuracy = 0.178;  // for Stratasys Dimension Elite
accuracy = 0.254;  // for Stratasys uPrint

accuracyRatio = accuracy / shapewaysAccuracy;

// Additional figures from Shapeways, but adjusted for the resolution of whatever printer we have.
// Assumed to be more or less analogous.
clearance = 0.5 * accuracyRatio;
minWallSupported = 0.7 * accuracyRatio;
minWallFree = 0.7 * accuracyRatio;
minWireSupported = 0.9 * accuracyRatio;
minWireFree = 1.0 * accuracyRatio;
minEmbossedDetail = 0.2 * accuracyRatio;
minEmbossedText = 0.5 * accuracyRatio;
minEngravedDetail = 0.2 * accuracyRatio;
minEngravedText = 0.5 * accuracyRatio;
//Min Bounding Box: x+y+z ≥ 7.5mm
//Max Bounding Box: 650x350x550mm (White) · 230x180x320mm (Black) · 150x150x150mm (Polished & Dyed)

accuracy2 = accuracy * 2;

// A safe gap between the printed part and each side of a foreign object such as a circuit board.
// We assume these have been directly measured with digital calipers (as opposed to relying on product specifications),
// with an accuracy of no less than 0.1mm.
foreignPartClearance = accuracy + 0.1;


////////////////////////////////////////////////////////////////////////////////
// dimensions of foreign parts

batteryWidth = 25;
batteryLength = 36;
batteryHeight = 5.4;

chargerHeaderLength = 5.1;
chargerHeaderWidth = 2.6;

laserWidth = 10.3;
laserThick = 3.3;

ledDomeRadius = 5.0 / 2;
ledRimRadius = 5.9 / 2;
ledRimThick = 1.0;

lightSensorRadius = 5.0 / 2;
lightSensorThick = 2.0;
lightSensorWireThick = 0.6;

modemWidth = 16.8;
modemLength = 42.8;
modemHeight = 4.1;

motionSensorWidth = 15.4;
motionSensorLength = 25.8;
motionSensorHeight = 2.5;

powerSwitchLength = 11.7;
powerSwitchWidth = 4.0;
powerSwitchDepth = 7.7;

pressureSensorWidth = 7.2;  // max value, from the data sheet
pressureSensorThick = 0.35;  // from the data sheet

pushButtonWellWidth = 7;
pushButtonHeight = 5.0;
pushButtonLegLength = 3.4;
pushButtonPressDepth = 0.25;
pushButtonWellDepth = 4.0;  // this is the height of the body of the push button (without the actual button)
pushButtonBaseThick = 1.5;

nanoLength = 43.2;
nanoWidth = 18.0;
nanoHeightWithoutUsb = 5.4;  // used only for visualization
nanoHeightToUsbBase = 3.4;
nanoUsbWidth = 7.7;
nanoUsbHeight = 3.8;
nanoUsbCableWidth = 9.6;
nanoUsbCableHeight = 6.6;
nanoUsbCableOffsetFromPerfboard = 3.3;

transducerWidth = 14.5;
transducerLength = 21.5;
transducerHeight = 7.9;


////////////////////////////////////////////////////////////////////////////////
// parameters of the printed part

// measurements taken from the clay model
thumbCurveRadius = 39.0;
offsetFromTopEdgeToFirstFinger = 15.0;
fingerWidth = 84 / 4;  // 21 -- 82mm was measured, but this seemed a little cramped
thumbWidth = 25;
thumbUpperRightX = 25.0;
fingerWellDepth = 5;
thumbWellDepth = 5;

// reasonably chosen values
caseWidth = 70;
caseLength = 120;
lidThick = 1.5;
floorThick = 1.5;

dividerThick = minWallSupported * 1.5;

// horizontal offset of components with ports/controls on the bottom edge
nanoOffset = 0;
chargerHeaderOffset = nanoOffset + nanoWidth + foreignPartClearance + dividerThick;
// place the power switch a reasonable distance past the charger header
powerSwitchOffset = chargerHeaderOffset + chargerHeaderWidth + foreignPartClearance + 5;

thickestComponent = transducerHeight;
cavityHeight = thickestComponent + foreignPartClearance;
caseHeight = lidThick + floorThick + cavityHeight;

rimThick = caseHeight/2;
// TODO: give the case a square profile by making caseCornerRadius less than half of the case height
caseCornerRadius = rimThick;

topYOfFingers = caseLength - offsetFromTopEdgeToFirstFinger + dividerThick/2;
bottomYOfFingers = caseLength - offsetFromTopEdgeToFirstFinger - 4*fingerWidth - dividerThick/2;

alignmentPegWidth = 2;
alignmentPegHeight = 2;

buttonHeight = 8;
buttonWidth = pushButtonWellWidth;
fingerButtonLength = fingerWidth - dividerThick;
thumbButtonLength = thumbWidth - dividerThick;
buttonClearance = accuracy;  // clearance between button and walls (on all four sides)
buttonLip = 2.5;
buttonSlack = pushButtonPressDepth * 2;  // minimum vertical clearance between button and floor/retainers
buttonStabilizerThick = 1.75; // vertical bars on buttons and case
buttonRetainerThick = 1.0; // horizontal bars on buttons and case
buttonRetainerGap = 0.75;
buttonRetainerHookHeight = pushButtonHeight - (pushButtonPressDepth + buttonSlack + accuracy2);

// buttons are inset by only 1/3 of the depth of the finger or thumb well
totalFingerWellDepth = fingerWellDepth/3 + buttonHeight + pushButtonHeight;
totalThumbWellDepth = thumbWellDepth/3 + buttonHeight + pushButtonHeight;

// this keeps the edges of the thumb container from protruding out of the case
thumbWellDepression = 2;

thumbUpperRightY = caseLength - thumbCurveRadius + sqrt(sq(thumbCurveRadius) - sq(thumbCurveRadius - thumbUpperRightX)) - thumbWellDepression;

pinHoleRadius = 0.7 * sqrt(accuracyRatio);
pinHoleBlockWidth = 2 * (pinHoleRadius + clearance*1.5);

// just to be on the safe side, we overestimate the necessary clearance
pressureSensorChannelHeight = clearance * 1.5;

cavityWidth = caseWidth - rimThick - totalFingerWellDepth - pushButtonBaseThick;
cavityLength = caseLength - 2*rimThick;

buttonLidHeight = (caseHeight-buttonWidth)/2;

// for visualization only
nanoVizOffsetX = 1;
nanoVizOffsetY = 1;
modemVizOffsetX = cavityWidth - 2 - 16;
modemVizOffsetY = nanoLength + dividerThick + 2;
motionSensorVizOffsetX = nanoOffset + nanoWidth + dividerThick + 3;
motionSensorVizOffsetY = 3;
batteryVizOffsetX = modemVizOffsetX - 1 - 25;
batteryVizOffsetY = modemVizOffsetY;
transducerVizOffsetX = motionSensorVizOffsetX;
transducerVizOffsetY = motionSensorVizOffsetY + 1 + 26;


////////////////////////////////////////////////////////////////////////////////
// modules

// creates a button of the given length.  Clearance on all sides of the button is subtracted.
// the long axis of the button is x.  y is side-to-side.  z is up and down (as you press the button).
module button(length) {
    innerRetainerWellWidth = (length - pushButtonWellWidth - foreignPartClearance)/2 - buttonStabilizerThick;
    retainerLength = innerRetainerWellWidth - buttonClearance - buttonRetainerGap;

    // this measurement is crucial
    stabilizerHeight
        = 2*buttonRetainerThick
        + (pushButtonHeight-pushButtonWellDepth+foreignPartClearance)
        + pressureSensorThick + 0.15  // TODO: improve this estimate when you have the sensor in hand
        + 0.1; // "a little extra" which can be easily corrected with tape or paint

    difference() {
        union() {
            // rounded cap
            translate([buttonClearance,buttonWidth/2,buttonWidth/2]) {
                rotate([0,90,0]) {
                    cylinder(h=length-2*buttonClearance,r=buttonWidth/2-buttonClearance, $fn=cornerRoundingRes);
                }
            }

            // button body
            translate([buttonClearance,buttonClearance,buttonWidth/2]) {
                cube([length-2*buttonClearance,buttonWidth-2*buttonClearance,buttonHeight-buttonWidth/2]);
            }

            // stabilizer bars
            translate([buttonClearance,buttonClearance,buttonHeight]) {
                cube([buttonStabilizerThick, buttonWidth-2*buttonClearance, stabilizerHeight]);
            }
            translate([length-buttonStabilizerThick-buttonClearance,buttonClearance,buttonHeight]) {
                cube([buttonStabilizerThick, buttonWidth-2*buttonClearance, stabilizerHeight]);
            }

            // retainer bars
            translate([buttonClearance,buttonClearance,buttonHeight+stabilizerHeight-buttonRetainerThick]) {
                cube([retainerLength,buttonWidth-2*buttonClearance, buttonRetainerThick]);
            }
            translate([length-buttonClearance-retainerLength,buttonClearance,buttonHeight+stabilizerHeight-buttonRetainerThick]) {
                cube([retainerLength,buttonWidth-2*buttonClearance, buttonRetainerThick]);
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
    w = pushButtonWellWidth + foreignPartClearance;
    l = 1.5; // width of leg holes

    outerRetainerWellWidth = buttonStabilizerThick + buttonRetainerGap;
    innerRetainerWellWidth = (length - pushButtonWellWidth - foreignPartClearance)/2 - buttonStabilizerThick;

    // the pressure sensor channels extend slightly past the push button well
    fsrChannelDepth = pushButtonLegLength + pushButtonHeight;

    translate([-w/2, -w/2, 0]) {

		// square well for body of switch
        cube([w, w, depth]);

        translate([0,0,-pushButtonLegLength]) {
            // channels for legs/wires of switch
            translate([0,0,0]) {
                cube([l,l,depth+pushButtonLegLength]);
            }
            translate([w-l,0, 0]) {
                cube([l,l,depth+pushButtonLegLength]);
            }
            translate([0,w-l, 0]) {
                cube([l,l,depth+pushButtonLegLength]);
            }
            translate([w-l,w-l, 0]) {
                cube([l,l,depth+pushButtonLegLength]);
            }
        }
    }

    // channels for pressure sensor
    // note: the slight 0.001 overlap prevents a confusing "film" in the STL
    translate([-(pressureSensorWidth+foreignPartClearance)/2,-w/2,-pushButtonLegLength]) {
        translate([0, -pressureSensorChannelHeight, 0]) {
            cube([pressureSensorWidth+foreignPartClearance, pressureSensorChannelHeight+0.001, fsrChannelDepth]);
        }
        translate([0, w-0.001, 0]) {
            cube([pressureSensorWidth+foreignPartClearance, pressureSensorChannelHeight, fsrChannelDepth]);
        }
    }

    // rim to allow the pressure sensor to wrap around
    translate([-(pressureSensorWidth+foreignPartClearance)/2, -w/2-pressureSensorChannelHeight, depth]) {
        cube([pressureSensorWidth+foreignPartClearance, w+2*pressureSensorChannelHeight, pushButtonHeight - pushButtonWellDepth]);
    }

    translate([-length/2,-buttonWidth/2,pushButtonWellDepth]) {
        // rectangular well for button body
        cube([length,buttonWidth,buttonHeight+10]);

        // retainer wells
        translate([0,0,-pushButtonWellDepth]) {
            cube([outerRetainerWellWidth, buttonWidth, pushButtonWellDepth]);

            cube([innerRetainerWellWidth, buttonWidth, pushButtonWellDepth - buttonRetainerThick]);

            translate([length-outerRetainerWellWidth,0,0]) {
                cube([outerRetainerWellWidth, buttonWidth, pushButtonWellDepth]);
            }

            translate([length-innerRetainerWellWidth,0,0]) {
                cube([innerRetainerWellWidth, buttonWidth, pushButtonWellDepth - buttonRetainerThick]);
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

    translate([radius - depth, 0, rimThick]) {
        cylinder(h=rimThick+.001, r1=radius-rimThick/2, r2=radius+rimThick/2, $fn=cornerRoundingRes);
    }
 
    translate([radius - depth,0,0]) {
        cylinder(h=rimThick+.001, r2=radius-rimThick/2, r1=radius+rimThick/2, $fn=cornerRoundingRes);
    }

    // well and channels for push button switch and wires
    translate([-totalWellDepth,0,caseHeight/2]) {
        rotate([0,90,0]) { rotate([0,0,90]) {
            pushButtonWell(buttonLength, pushButtonWellDepth);
        }}
    }
}

module fingerContainer(totalWellDepth, buttonLength) {
    translate([-totalWellDepth-pushButtonBaseThick, -buttonLength/2-dividerThick, lidThick]) {
        cube([totalWellDepth+pushButtonBaseThick, buttonLength + 2*dividerThick, caseHeight-lidThick]);
    }
}

module buttons() {
    fingerButtonsPrintable = [caseWidth + 15,bottomYOfFingers+dividerThick,10];
    fingerButtonsInSitu = [caseWidth - fingerWellDepth * 1/3,bottomYOfFingers + dividerThick,buttonLidHeight + buttonWidth];
    thumbButtonPrintable = [thumbUpperRightX+thumbButtonLength+dividerThick,caseLength+12,10];
    thumbButtonInSitu = [thumbUpperRightX + thumbWidth - dividerThick/2,thumbUpperRightY - fingerWellDepth * 1/3, buttonLidHeight + buttonWidth];

    // TODO: simplify this ridiculous if..then
    if (buttonsInSitu) {
        // Note: the buttons are oriented so as to make the longest surfaces the smoothest
        translate(fingerButtonsInSitu) {
            for (i = [0:3]) {
                translate([0,i*fingerWidth,0]) { rotate([90,180,-90]) {
                    button(fingerButtonLength);
                }}
            }
        }
        translate(thumbButtonInSitu) {
            rotate([90,180,0]) {
                button(thumbButtonLength);
            }
        }
    } else {
        // Note: the buttons are oriented so as to make the longest surfaces the smoothest
        translate(fingerButtonsPrintable) {
            for (i = [0:3]) {
                translate([0,i*fingerWidth,0]) { rotate([90,180,-90]) {
                    button(fingerButtonLength);
                }}
            }
        }
        translate(thumbButtonPrintable) {
            rotate([90,180,0]) {
                button(thumbButtonLength);
            }
        }
    }
}

// for the thumb rest
module roundedCylinder(height, radius, cornerRadius) {
    cylinder(r=(thumbCurveRadius - caseCornerRadius), h=caseHeight);
    translate([0, 0, caseCornerRadius]) {
        rotate_extrude($fn=thumbCurveRes) {
            translate([thumbCurveRadius - caseCornerRadius, 0, 0]) {
                circle(r = caseCornerRadius, $fn=cornerRoundingRes);
            }
        }
        cylinder(r=thumbCurveRadius, h=(caseHeight - 2*caseCornerRadius), $fn=cornerRoundingRes);
    }
    translate([0, 0, caseHeight - caseCornerRadius]) {
        rotate_extrude() {
            translate([thumbCurveRadius - caseCornerRadius, 0, 0]) {
                circle(r = caseCornerRadius, $fn=cornerRoundingRes);
            }
        }
    } 
}

// for the rounded flat edges of the case
module roundedEdge(length, height, cornerRadius) {
    rem = height - 2*cornerRadius;

    if (simpleEdges) {
        cylinder(r=cornerRadius, h=length, $fn=cornerRoundingRes);
    } else {
        intersection() {
            translate([-height/2, 0, 0]) {
                cube([height, cornerRadius, length]);
            }
            union() {
                translate([-rem/2.0, 0, 0]) {
                    cylinder(h=length, r=cornerRadius, $fn=cornerRoundingRes);
                }
                translate([rem/2.0, 0, 0]) {
                    cylinder(h=length, r=cornerRadius, $fn=cornerRoundingRes);
                }
                translate([-rem/2.0, -cornerRadius, 0]) {
                    cube([rem, 2 * cornerRadius, length]);
                }
            }
        }
    }
}

// for the intersections between rounded edges
module roundedCorner(height, cornerRadius) {
    if (simpleEdges) {
        sphere(r=cornerRadius, $fn=cornerRoundingRes);
    } else {
        difference() {
            union() {
                translate([0, 0, -height/2.0 + cornerRadius]) {
                    sphere(r=cornerRadius, $fn=cornerRoundingRes);
                    cylinder(r=cornerRadius, h=(height - 2*cornerRadius), $fn=cornerRoundingRes);
                }
                translate([0, 0, height/2.0 - cornerRadius]) {
                    sphere(r=cornerRadius, $fn=cornerRoundingRes);
                }
            }
            translate([0, 0, -height/2]) {
                cube([cornerRadius, cornerRadius, height]);
            }
        }
    }
}

module pinHole() {
    translate([0,0,-1]) {
        cylinder(h=caseHeight+2,r=pinHoleRadius, $fn=pinHoleRes);
    }
}

module pinHoles() {
    // pinholes on the straight portion of the right edge
    for (i = [1:4]) {
        translate([rimThick*2/3, rimThick*2/3 + i*(caseLength-thumbCurveRadius - rimThick*2/3)/4, 0]) {
            pinHole();
        }
    }

    // pinholes close to the corners
    translate([rimThick*2/3,rimThick+pinHoleRadius,0]) {
        pinHole();
    }
    translate([caseWidth-rimThick*2/3,rimThick+pinHoleRadius,0]) {
        pinHole();
    }
    translate([caseWidth-rimThick-pinHoleRadius,caseLength-rimThick*2/3,0]) {
        pinHole();
    }
    translate([caseWidth-rimThick*2/3,caseLength-rimThick-pinHoleRadius,0]) {
        pinHole();
    }
    translate([caseWidth-rimThick-pinHoleRadius,rimThick*2/3,0]) {
        pinHole();
    }

    // pinholes on the bottom edge which avoid interfering with the battery charger port, power switch, and USB port
    translate([rimThick,0,0]) {
        translate([nanoOffset+pinHoleRadius,rimThick*2/3,0]) {
            pinHole();
        }
        translate([nanoOffset+nanoWidth-pinHoleRadius,rimThick*2/3,0]) {
            pinHole();
        }
        translate([powerSwitchOffset-pinHoleRadius,rimThick*2/3,0]) {
            pinHole();
        }
        translate([powerSwitchOffset+powerSwitchLength+2*dividerThick+2*foreignPartClearance+pinHoleRadius,rimThick*2/3,0]) {
            pinHole();
        }
    }

    // pinholes on curved surface of thumb rest
    translate([thumbCurveRadius, caseLength-thumbCurveRadius, 0]) {
        rotate([0,0,-20]) {
            translate([-thumbCurveRadius+rimThick*2/3,0,0]) {
                pinHole();
            }
        }
        rotate([0,0,-40]) {
            translate([-thumbCurveRadius+rimThick*2/3,0,0]) {
                pinHole();
            }
        }
        rotate([0,0,-60]) {
            translate([-thumbCurveRadius+rimThick*2/3,0,0]) {
                pinHole();
            }
        }
    }

    // finger pinholes
    translate([rimThick + cavityWidth + pinHoleBlockWidth/2,caseLength - offsetFromTopEdgeToFirstFinger + dividerThick/2,0]) {
        pinHole();
    }
    for (i = [1:3]) {
        translate([
            rimThick + cavityWidth,
            caseLength - offsetFromTopEdgeToFirstFinger - fingerWidth * i,
            0]) {
            pinHole();
        }
    }
    translate([rimThick+cavityWidth, caseLength-offsetFromTopEdgeToFirstFinger - fingerWidth*4 + pinHoleBlockWidth/2, 0]) {
        pinHole();
    }

    // thumb pinholes
    translate([thumbUpperRightX+1.5, thumbUpperRightY-totalFingerWellDepth-pinHoleBlockWidth/2, 0]) {
        pinHole();
    }
    translate([thumbUpperRightX+thumbWidth-1.5, thumbUpperRightY-totalFingerWellDepth-pinHoleBlockWidth/2, 0]) {
        pinHole();
    }
}

module lightSensorWell() {
    translate([caseWidth + 1, ((caseLength - rimThick) + topYOfFingers) / 2, caseHeight / 2]) {
        rotate([0,-90,0]) { rotate([0,0,90]) {
            cylinder(r=(lightSensorRadius+foreignPartClearance), h=(lightSensorThick+foreignPartClearance + 1), $fn=lightSensorWellRes);
            translate([-lightSensorRadius, -(lightSensorWireThick + foreignPartClearance)/2, 0]) {
                cube([lightSensorRadius*2, lightSensorWireThick + foreignPartClearance, rimThick + 2]);
            }
        }}
    }
}

module statusLEDHole() {
    translate([caseWidth - rimThick - 5.5, caseLength, caseHeight/2]) {
        rotate(a=[90,0,0]) {
            cylinder(h=rimThick,r=ledDomeRadius+foreignPartClearance, $fn=ledDomeRes);

            // note: the rim depression uses a larger accuracy/clearance, since it doesn't need
            // to fit tightly and can't be filed down
            translate([0,0,-2+rimThick]) {
                cylinder(h=5,r=ledRimRadius+foreignPartClearance, $fn=ledRimRes);
            }
        }
    }
}

module caseConvexHull() {
    // box with cutout for thumb cylinder
    difference() {
        translate([rimThick, rimThick,0]) {
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
                cylinder(r=(thumbCurveRadius - caseCornerRadius), h=cavityHeight);
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

module thumbButtonContainer() {
    translate([thumbUpperRightX, thumbUpperRightY, 0]) {
        rotate([0,0,90]) {
            translate([0,-(thumbButtonLength+dividerThick)/2],0){
                fingerContainer(totalFingerWellDepth, thumbButtonLength);
            }
        }
    }
}

module thumbButtonWell() {
    translate([thumbUpperRightX, thumbUpperRightY, 0]) {
        rotate([0,0,90]) {
            translate([0,-(thumbButtonLength+dividerThick)/2],0){
                fingerWell(thumbWellDepth, thumbWidth, totalFingerWellDepth, thumbButtonLength);
            }
        }
    }

    // take an extra "bite" out of the cusp to the right of the thumb button, which would otherwise get in the way
    translate([thumbUpperRightX - 10, thumbUpperRightY + thumbWellDepression -1, 0]) {
        cube([12, 5, caseHeight]);
    }
}

module basicCase() {
    difference() {
        union() {
            difference() {
                caseConvexHull();

                // inner cavity
                translate([0,0,lidThick]) {
                    translate([rimThick,rimThick,0]) {
                        cube([cavityWidth,caseLength-thumbCurveRadius-rimThick+0.001,cavityHeight]);
                    }
                    translate([thumbCurveRadius,rimThick,0]) {
                        cube([cavityWidth+rimThick-thumbCurveRadius,cavityLength,cavityHeight]);
                    }
                    translate([rimThick+cavityWidth - 0.001,topYOfFingers,0]) {
                        cube([caseWidth - 2*rimThick - cavityWidth, caseLength - rimThick - topYOfFingers, cavityHeight]);
                    }
                    translate([rimThick+cavityWidth - 0.001,rimThick,0]) {
                        cube([caseWidth - 2*rimThick - cavityWidth, bottomYOfFingers - rimThick, cavityHeight]);
                    }
                }
            }

            thumbButtonContainer();

            // blocks for finger pinholes
            translate([rimThick + cavityWidth,caseLength - offsetFromTopEdgeToFirstFinger + dividerThick/2 - pinHoleBlockWidth/2,0]) {
                cube([pinHoleBlockWidth,pinHoleBlockWidth,caseHeight]);
            }
            for (i = [1:3]) {
                translate([
                    rimThick + cavityWidth - pinHoleBlockWidth/2,
                    caseLength - offsetFromTopEdgeToFirstFinger - fingerWidth * i - pinHoleBlockWidth/2,
                    0]) {
                    cube([pinHoleBlockWidth,pinHoleBlockWidth,caseHeight]);
                }
            }
            translate([rimThick+cavityWidth - pinHoleBlockWidth/2, caseLength-offsetFromTopEdgeToFirstFinger - fingerWidth*4, 0]) {
                cube([pinHoleBlockWidth,pinHoleBlockWidth,caseHeight]);
            }

            // blocks for thumb pinholes
            translate([thumbUpperRightX, thumbUpperRightY-totalFingerWellDepth-pinHoleBlockWidth, 0]) {
                cube([pinHoleBlockWidth,pinHoleBlockWidth,caseHeight]);
            }
            translate([thumbUpperRightX+thumbWidth-pinHoleBlockWidth, thumbUpperRightY-totalFingerWellDepth-pinHoleBlockWidth, 0]) {
                cube([pinHoleBlockWidth,pinHoleBlockWidth,caseHeight]);
            }
        }

        pinHoles();

        // finger button wells
        for (i = [0:3]) {
            translate([
                caseWidth,
                caseLength - offsetFromTopEdgeToFirstFinger - fingerWidth * (i + 0.5),
                0]) {
                    fingerWell(fingerWellDepth, fingerWidth, totalFingerWellDepth, fingerButtonLength);
            }
        }

        thumbButtonWell();

        lightSensorWell();

        statusLEDHole();

        // holes for ports/controls on bottom edge
        translate([rimThick, 0, 0]) {
            // hole for Arduino USB port
            translate([nanoOffset+(nanoWidth+foreignPartClearance-nanoUsbWidth)/2,0,caseHeight-floorThick-nanoHeightToUsbBase-nanoUsbHeight-foreignPartClearance]) {
                cube([nanoUsbWidth+foreignPartClearance,rimThick+1,nanoUsbHeight+foreignPartClearance]);
            }

            translate([nanoOffset+(nanoWidth+foreignPartClearance-nanoUsbCableWidth)/2,0,caseHeight-floorThick-nanoHeightToUsbBase-(nanoUsbHeight+nanoUsbCableHeight+foreignPartClearance)/2]) {
                cube([nanoUsbCableWidth+foreignPartClearance,rimThick-nanoUsbCableOffsetFromPerfboard,nanoUsbCableHeight+foreignPartClearance]);
            }

            // hole for battery charger headers
            translate([chargerHeaderOffset,0,(caseHeight-chargerHeaderLength-foreignPartClearance)/2]) {
                cube([chargerHeaderWidth+foreignPartClearance,rimThick+1,chargerHeaderLength+foreignPartClearance]);
            }

            // hole for SPDT Mini Power Switch
            translate([powerSwitchOffset+dividerThick,0,(caseHeight-powerSwitchWidth-foreignPartClearance)/2]) {
                cube([powerSwitchLength+foreignPartClearance,rimThick+1,powerSwitchWidth+foreignPartClearance]);
            }
        }

        // cutaway for laser shelf
        translate([caseWidth - rimThick - 1, bottomYOfFingers - laserWidth - foreignPartClearance, lidThick]) {
            cube([rimThick + 2, laserWidth + foreignPartClearance, laserThick + foreignPartClearance]);
        }
    }
}

module alignmentPeg() {
    cube([alignmentPegWidth,alignmentPegWidth,buttonLidHeight - lidThick]);
    translate([accuracy, accuracy, 0]) {
        cube([alignmentPegWidth-accuracy2,alignmentPegWidth-accuracy2,alignmentPegHeight]);
    }
}


////////////////////////////////////////////////////////////////////////////////
// main

echo("accuracy: ", accuracy);
echo("clearance: ", clearance);
echo("foreignPartClearance ", foreignPartClearance);
echo("caseWidth: ", caseWidth);
echo("caseLength: ", caseLength);
echo("caseHeight: ", caseHeight);
echo("rimThick: ", rimThick);
echo("cavityWidth: ", cavityWidth);
echo("cavityLength: ", cavityLength);
echo("cavityHeight: ", cavityHeight);
echo("pinHoleRadius: ", pinHoleRadius);
echo("pinHoleBlockWidth: ", pinHoleBlockWidth);

// body
difference() {
    basicCase();

    cube([caseWidth, caseLength, buttonLidHeight+0.0001]);

    // screwdriver/leverage slot(s)
    translate([rimThick+cavityWidth,rimThick/2,buttonLidHeight]) {
        rotate([150,0,0]) { cube([5,10,10]); }
    }
}

// lid
translate([-5, 0, 8]) {
    rotate([0,180,0]) {
        difference() {
            basicCase();
            translate([0,0,buttonLidHeight]) {
                cube([caseWidth, caseLength, caseHeight]);
            }
        }

        // alignment pegs
        translate([rimThick, rimThick, lidThick]) { alignmentPeg(); }
        translate([caseWidth-rimThick-alignmentPegWidth, rimThick, lidThick]) { alignmentPeg(); }
        translate([caseWidth-rimThick-alignmentPegWidth, caseLength-rimThick-alignmentPegWidth, lidThick]) { alignmentPeg(); }
    }
}

buttons();

// electronic components
translate([rimThick,rimThick,0]) {

    // container for Arduino Nano v3.0
    translate([nanoOffset,0,caseHeight-floorThick-nanoHeightToUsbBase]) {
        // vertical wall
        translate([nanoWidth+foreignPartClearance,0,0]) {
            cube([dividerThick,nanoLength+foreignPartClearance+dividerThick,nanoHeightToUsbBase]);
        }
        // horizontal wall
        translate([0,nanoLength+foreignPartClearance,0]) {
            cube([nanoWidth+foreignPartClearance+dividerThick,dividerThick,nanoHeightToUsbBase]);
        }
    }

    // container for power switch
    translate([powerSwitchOffset,-0.1,(caseHeight-powerSwitchWidth-foreignPartClearance)/2]) {
        // right vertical wall
        cube([dividerThick,powerSwitchDepth-rimThick+foreignPartClearance+dividerThick+0.1,(caseHeight+powerSwitchWidth+foreignPartClearance)/2-floorThick]);
        // left vertical wall
        translate([dividerThick+powerSwitchLength+foreignPartClearance,0,0]) {
            cube([dividerThick,powerSwitchDepth-rimThick+foreignPartClearance+dividerThick+0.1,(caseHeight+powerSwitchWidth+foreignPartClearance)/2-floorThick]);
        }
        // base
        translate([0,0,powerSwitchWidth+foreignPartClearance]) {
            cube([powerSwitchLength+foreignPartClearance+2*dividerThick,powerSwitchDepth-rimThick+foreignPartClearance+dividerThick+0.1,(caseHeight-powerSwitchWidth-foreignPartClearance)/2-floorThick]);
        }
        // rear support
        translate([0,powerSwitchDepth-rimThick+foreignPartClearance+0.1,powerSwitchWidth+foreignPartClearance-1]) {
            cube([powerSwitchLength+foreignPartClearance+2*dividerThick,dividerThick,1]);
        }
    }

    // shelf for the laser
    translate([cavityWidth, bottomYOfFingers - laserWidth - foreignPartClearance - rimThick, lidThick + laserThick + foreignPartClearance]) {
        cube([caseWidth-2*rimThick-cavityWidth, laserWidth + foreignPartClearance, dividerThick]);
    }

    if (visualizeInternalComponents) {
        // Arduino Nano v3.0
        translate([nanoVizOffsetX,nanoVizOffsetY,-10]) {
            cube([nanoWidth,nanoLength,nanoHeightWithoutUsb]);
        }

        // Surface Transducer - Small
        translate([transducerVizOffsetX,transducerVizOffsetY,-10]) {
           cube([transducerLength,transducerWidth,transducerHeight]);
        }
        // Bluetooth Modem - BlueSMiRF Silver
        translate([modemVizOffsetX,modemVizOffsetY,-10]) { cube([modemWidth,modemLength,modemHeight]); }
        // Polymer Lithium Ion Battery - 110mAh
        //translate([-40,0,-10]) { cube([12,28,5.7]); }
        // Polymer Lithium Ion Battery - 400mAh
	    translate([batteryVizOffsetX,batteryVizOffsetY,-10]) { cube([batteryWidth,batteryLength,batteryHeight]); }
        // Triple Axis Accelerometer & Gyro Breakout - MPU-6050
        translate([motionSensorVizOffsetX,motionSensorVizOffsetY,-10]) { cube([motionSensorWidth, motionSensorLength, motionSensorHeight]); }
    }
}
