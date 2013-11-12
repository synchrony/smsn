// The origin is at the "bottom right" of the case (in the empty space where the lid goes),
// with the thumb to the north (the y axis), the fingers of the left hand to the left
// (the x axis) and pointing away, and the finger joystick well on the opposite side of the
// case pointing down (the z axis) away from the palm.

function pythag(a, b) = sqrt(a*a + b*b);
function angle(a, b) = atan(a/b);

visualizeInternalComponents = false;

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

thumbCurveRadius = 39.0;
caseCornerRadius = 6;
// note: the metal casing on one axis of the joystick makes it a little wider (around 18.8)
joystickWidth = 18.5;
joystickBodyHeight = 5.4;

joystickWellDepth = joystickBodyHeight;
fingerJoystickWellLength = 90;

nanoLength = 43.2;
nanoWidth = 18.0;
nanoHeightWithoutUsb = 5.4;
nanoHeightToUsbBase = 3.3;
nanoUsbWidth = 7.6;
nanoUsbHeight = 3.9;
nanoLeftMargin = 5;

chargerHeaderLength = 5.1;
chargerHeaderWidth = 2.6;

powerSwitchLength = 11.6;
powerSwitchWidth = 4.0;
powerSwitchDepth = 7.5;
powerSwitchOffset = 12;

ledDomeRadius = 2.5;
ledRimRadius = 2.95;
ledRimThick = 1.0;

lidThick = 1.5;
floorThick = 1.5;
containmentWallThick = 1;
dividerThick = 1.5;
lidEdgeThick = 5.75; // FIXME: this is pretty arbitrary, based on a Mark 1 calculation which no longer applies

// based on the modeling clay prototype (just over 49mm)
caseWidth = 50;

// based on the modeling clay prototype, but may be increased if necessary
caseLength = 117;

thickestComponent = 7.9;
//caseHeight = lidThick + floorThick + thickestComponent + 1;
//caseHeight = joystickWidth;
caseHeight = max(joystickWidth, joystickWellDepth + dividerThick + thickestComponent + lidThick + 1);
cavityDepth = caseHeight - joystickWellDepth - dividerThick;

wallThick = caseHeight/2;
//wallThick = min(6, caseHeight/2);

echo("caseHeight: ", caseHeight);

pushButtonWellWidth = 7;
pushButtonHeight = 5.0;
pushButtonLegLength = 3.4;
pushButtonPressDepth = 0.25;
pushButtonWellDepth = 4.0;
pushButtonBaseThick = 1.5;

fingerHeightOffset = 5;
fingerWellDepth = 10;
fingerRad = 15;
thumbWellDepth = wallThick;
thumbWellRadius = 30;
buttonThick = 8;
buttonWidth = pushButtonWellWidth;
fingerButtonLength = 25;
thumbButtonLength = 30;
buttonStabilizerWellWidth = 3;
buttonClearance = 0.3;  // horizontal clearance between button and walls
buttonLip = 2.5;
buttonSlack = 0.5;  // minimum vertical clearance between button and floor/retainers
buttonRetainerThick = 1.0;
buttonStabilizerRodHeight = pushButtonHeight - (pushButtonPressDepth + buttonSlack + error2);
totalFingerWellDepth = fingerWellDepth + buttonThick/2 + pushButtonPressDepth + pushButtonWellDepth;
totalThumbWellDepth = thumbWellDepth + buttonThick/2 + pushButtonPressDepth + pushButtonWellDepth;

pinHoleRadius = 0.8;

thumbBevelLength = 48;
thumbBevelWidth = 31;
thumbBevelBuffer = 2;

thumbBevelAngle = angle(thumbBevelLength-wallThick,thumbBevelWidth-wallThick);
thumbBevelStretch = pythag(thumbBevelLength-wallThick, thumbBevelWidth-wallThick);

thumbKeyOffset = 27;

cavityWidth = caseWidth - wallThick - totalFingerWellDepth - pushButtonBaseThick;
cavityLength = caseLength - (2 * wallThick);

module pinHole() {
    cylinder(h=caseHeight+2,r=pinHoleRadius, $fn=pinHoleRes);
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

module pinHoles() {
    translate([0,0,-1]) {

        for (i = [0:4]) {
            translate([wallThick+cavityWidth+3,caseLength-fingerHeightOffset-fingerRad*i*2,0]) { pinHole(); }
        }

		// pin holes along palmar edge
        for (i = [1:4]) {
            translate([caseCornerRadius*2/3,caseCornerRadius*2/3 + i*(caseLength-thumbCurveRadius - caseCornerRadius)/4,0]) {
                pinHole();
            }
        }
        translate([12,caseLength-thumbBevelLength+2,0]) {
            pinHole();
        }
        translate([thumbBevelWidth, caseLength-wallThick*2/3,0]) {
            pinHole();
        }
        translate([thumbBevelWidth, caseLength-15, 0]) {
            pinHole();
        }
        translate([wallThick*2/3,10,0]) {
            pinHole();
        }
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
		// box with thumb cutout
//        difference() {
//            translate([wallThick, wallThick,0]) {
//                cube([caseWidth-caseHeight,caseLength-caseHeight,caseHeight]);
//            }

//			translate([0, caseLength - thumbCurveRadius, 0]){
//				cube([thumbCurveRadius, thumbCurveRadius, caseHeight]);
//			}
//        }

    // thumb cylinder
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

module basicCase() {
  difference() {
    caseConvexHull();

	// well for finger joysticks
    translate([caseCornerRadius, caseCornerRadius, caseHeight - joystickWellDepth]) {
        cube([caseWidth - caseCornerRadius + 1, fingerJoystickWellLength, joystickWellDepth + 1]);
    }   

    pinHoles();

    // hole for status LED
	translate([thumbCurveRadius, caseLength - thumbCurveRadius, cavityDepth - ledRimRadius/2.0]) {
        rotate([0, 0, 45]) { translate([0, thumbCurveRadius, 0]) {
            rotate(a=[90,0,0]) {

                cylinder(h=caseCornerRadius+2,r=ledDomeRadius+error, $fn=ledHoleRes);

                // note: the rim depression uses a larger error/clearance, since it doesn't need
                // to fit tightly and can't be filed down
                translate([0,0,-1+caseCornerRadius+ledRimThick]) {
                    cylinder(h=10,r=ledRimRadius+clearance, $fn=20);
                }
            }
        }} 
    }

    // hole for battery charger headers
	translate([wallThick+cavityWidth-chargerHeaderWidth-error2,0,caseHeight-joystickWellDepth-dividerThick-chargerHeaderLength-error2]) {
        cube([chargerHeaderWidth+error2,wallThick+1,chargerHeaderLength+error2]);
    }

    // hole for SPDT Mini Power Switch
    translate([powerSwitchOffset,0,wallThick-powerSwitchWidth/2-error]) {
        cube([powerSwitchLength+error2,wallThick+1,powerSwitchWidth+error2]);
    }

    // hole for USB port
    translate([wallThick+cavityWidth-nanoLeftMargin-nanoWidth/2-nanoUsbWidth/2-clearance,0,caseHeight-joystickWellDepth-dividerThick-nanoHeightToUsbBase-nanoUsbHeight-clearance]) {
        cube([nanoUsbWidth+2*clearance,wallThick+1,nanoUsbHeight+clearance]);
    }
  }
}

// body
difference() {
    union() {
        basicCase();
        // receiver for stabilizer pin
        translate([caseWidth - caseCornerRadius, caseLength - caseCornerRadius, 0]) {
            cylinder(h=caseHeight-floorThick+0.001, r=3);
        }
    }

    cube([caseWidth, caseLength, (caseHeight-buttonWidth)/2+0.0001]);

    for (i = [0:4]) {
        translate([wallThick+cavityWidth+10,caseLength-fingerHeightOffset-fingerRad*i*2,0]) {
            cylinder(h=caseHeight*2/3, r=1.5+error, $fn=connectorPinRes);
        }
    }
    translate([thumbBevelWidth-3, caseLength-wallThick-6, 0]) {
        cylinder(h=caseHeight*2/3, r=1.5+error, $fn=connectorPinRes);
    }
    translate([wallThick+2, caseLength-thumbBevelLength+3, 0]) {
        cylinder(h=caseHeight*2/3, r=1.5+error, $fn=connectorPinRes);
    }
    translate([wallThick, wallThick, 0]) {
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
        translate([0, 0, lidEdgeThick]) {
            cube([caseWidth, caseLength, caseHeight]);
        }
    }

    // lid cover
    translate([caseCornerRadius, caseCornerRadius, 0]) {
        cube([caseWidth - 2*caseCornerRadius, caseLength - thumbCurveRadius - caseCornerRadius, lidThick]);
    }
	translate([thumbCurveRadius, caseLength - thumbCurveRadius, 0]) {
        cube([caseWidth - thumbCurveRadius - caseCornerRadius, thumbCurveRadius - caseCornerRadius, lidThick]);
    }

    for (i = [0:4]) {
        translate([wallThick+cavityWidth+10,caseLength-fingerHeightOffset-fingerRad*i*2,lidThick]) {
            cylinder(h=caseHeight/3, r=1.5, $fn=connectorPinRes);
        }
    }
    translate([thumbBevelWidth-3, caseLength-wallThick-6, lidThick]) {
        cylinder(h=caseHeight/3, r=1.5, $fn=connectorPinRes);
    }
    translate([wallThick+2, caseLength-thumbBevelLength+3, lidThick]) {
        cylinder(h=caseHeight/3, r=1.5, $fn=connectorPinRes);
    }
    translate([wallThick, wallThick, lidThick]) {
        cylinder(h=caseHeight/3, r=1.5, $fn=connectorPinRes);
    }
}

// electronic components
translate([wallThick,wallThick,0]) {
    // container for power switch
    translate([powerSwitchOffset-wallThick,0,wallThick-powerSwitchWidth/2-error]) {
        translate([-containmentWallThick,0,0]) {
            cube([containmentWallThick,powerSwitchDepth-wallThick+error+containmentWallThick,caseHeight-floorThick-wallThick+powerSwitchWidth/2+error]);
        }
        translate([powerSwitchLength+error2,0,0]) {
            cube([containmentWallThick,powerSwitchDepth-wallThick+error+containmentWallThick,caseHeight-floorThick-wallThick+powerSwitchWidth/2+error]);
        }
        translate([-containmentWallThick,0,powerSwitchWidth+2*error]) {
            cube([powerSwitchLength+2*(containmentWallThick+error),powerSwitchDepth-wallThick+error+containmentWallThick,caseHeight-floorThick-wallThick-powerSwitchWidth/2-error]);
        }
        translate([-containmentWallThick,powerSwitchDepth-wallThick+error,powerSwitchWidth+2*error-1]) {
            cube([powerSwitchLength+2*(containmentWallThick+error),containmentWallThick,1]);
        }
    }

    // container for Arduino Nano v3.0
    translate([cavityWidth-nanoLeftMargin-nanoWidth,0,0]) {
        translate([0,0,caseHeight-joystickWellDepth-dividerThick-nanoHeightToUsbBase-error]) {
            translate([-containmentWallThick-clearance,0,0]) {
                cube([containmentWallThick,nanoLength+clearance+containmentWallThick,nanoHeightToUsbBase+error]);
            }
            translate([nanoWidth+clearance,0,0]) {
                cube([containmentWallThick,nanoLength+clearance+containmentWallThick,nanoHeightToUsbBase+error]);
            }
            translate([-containmentWallThick-clearance,nanoLength+clearance,0]) {
                cube([nanoWidth+2*(clearance+containmentWallThick),containmentWallThick,nanoHeightToUsbBase+error]);
            }
        }
    }

    if (visualizeInternalComponents) {
        // Arduino Nano v3.0
        translate([cavityWidth-nanoLeftMargin-nanoWidth,0,-10]) {
            cube([nanoWidth,nanoLength,nanoHeightWithoutUsb]);
        }

        // Surface Transducer - Small
        translate([thumbBevelWidth-wallThick+4,caseLength-2*wallThick-21.4-4,-10]) {
           cube([13.8,21.4,7.9]);
        }
        // Bluetooth Modem - BlueSMiRF Silver -- 42.0 x 16.0 x 3.9
        translate([0,31,-10]) { cube([16,42,3.9]); }
        // Polymer Lithium Ion Battery - 110mAh
        translate([-40,0,-10]) { cube([12,28,5.7]); }
        // Polymer Lithium Ion Battery - 400mAh
	    translate([18,47,-10]) { cube([25,35,5]); }
        // Triple Axis Accelerometer & Gyro Breakout - MPU-6050
        translate([0,3,-10]) { cube([15.5, 25.7, 2.5]); }
    }
}
