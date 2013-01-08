
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
cornerRoundingRes = 50;
connectorPinRes = 50;
ledHoleRes = 50;
//*/
//*
cornerRoundingRes = 10;
connectorPinRes = 10;
ledHoleRes = 10;
//*/
containmentWallThick = 1;

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

caseWidth = 70;
caseLength = 130;
thickestComponent = 7.9;
caseHeight = lidThick + floorThick + thickestComponent + 1;

wallThick = caseHeight/2;

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

module fingerWell(depth, radius) {
    translate([radius - depth,0,0]) {
        cylinder(h=caseHeight,r=radius, $fn=cornerRoundingRes);
    }

    translate([radius - depth, 0, wallThick]) {
        cylinder(h=wallThick+.001, r1=radius-wallThick/2, r2=radius+wallThick/2, $fn=cornerRoundingRes);    
    }

    translate([radius - depth,0,0]) {
        cylinder(h=wallThick+.001, r2=radius-wallThick/2, r1=radius+wallThick/2, $fn=cornerRoundingRes);    
    }
}

module pinHole() {
    cylinder(h=caseHeight+2,r=pinHoleRadius, $fn=pinHoleRes);
}

module pinHoles() {
    translate([0,0,-1]) {

        for (i = [0:4]) {
            translate([wallThick+cavityWidth+3,caseLength-fingerHeightOffset-fingerRad*i*2,0]) { pinHole(); }
            //translate([wallThick+cavityWidth+10,caseLength-fingerHeightOffset-fingerRad*i*2,0]) { pinHole(); }

        }

        for (i = [1:4]) {
            translate([wallThick*2/3,wallThick*2/3 + i*(caseLength-thumbBevelLength - wallThick*2/3)/4,0]) {
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
		// box with thumb bevel
        difference() {
            translate([wallThick, wallThick,0]) {
                cube([caseWidth-caseHeight,caseLength-caseHeight,caseHeight]);
            }
            translate([thumbBevelWidth,caseLength-wallThick,-1]) {
                rotate([0,0,thumbBevelAngle]) {
                    rotate([0,-90,0]) { 
                        cube([caseHeight+1, 100, thumbBevelStretch]);
                    }
                }
            }
        }

        translate([wallThick,wallThick,wallThick]) {
            sphere(wallThick, $fn=cornerRoundingRes);
            rotate([-90,0,0]) {
                cylinder(h=caseLength-thumbBevelLength-wallThick, r=wallThick, $fn=cornerRoundingRes);
            }
            rotate([0,90,0]) {
                cylinder(h=caseWidth-caseHeight, r=wallThick, $fn=cornerRoundingRes);
            }
        }
        translate([caseWidth-wallThick,wallThick,wallThick]) {
            sphere(wallThick, $fn=cornerRoundingRes);
            rotate([-90,0,0]) {
                cylinder(h=caseLength-caseHeight, r=wallThick, $fn=cornerRoundingRes);
            }
        }
        translate([caseWidth-wallThick,caseLength-wallThick,wallThick]) {
            sphere(wallThick, $fn=cornerRoundingRes);
        }

		// thumb bevel
		translate([wallThick,caseLength-thumbBevelLength,wallThick]) {
            sphere(wallThick, $fn=cornerRoundingRes);
        }
        translate([thumbBevelWidth,caseLength-wallThick,wallThick]) {
            sphere(wallThick, $fn=cornerRoundingRes);
            rotate([0,90,0]) {
                cylinder(h=caseWidth-thumbBevelWidth-wallThick, r=wallThick, $fn=cornerRoundingRes);
            }
            rotate([0,0,180+thumbBevelAngle]) {
                rotate([0,90,0]) { 
                    cylinder(h=thumbBevelStretch, r=wallThick, $fn=cornerRoundingRes);
                }
            }
        }
}

module basicCase() {
  difference() {
    caseConvexHull();

    // inner compartment
    translate([wallThick,wallThick,lidThick]) {
        cube([cavityWidth,caseLength-thumbBevelLength-wallThick,caseHeight-floorThick-lidThick]);
    }
    translate([thumbBevelWidth+thumbBevelBuffer,wallThick,lidThick]) {
        cube([cavityWidth-thumbBevelWidth+wallThick-thumbBevelBuffer,cavityLength,caseHeight-floorThick-lidThick]);
    }

    pinHoles();

    // finger button wells
    for (i = [0:3]) {
        translate([
            caseWidth,
            caseLength - fingerHeightOffset - fingerRad * (1 + 2 * i),
        0]) {

            fingerWell(fingerWellDepth, fingerRad);

            // well and channels for push button switch and wires
            translate([-totalFingerWellDepth,0,caseHeight/2]) {
                rotate([0,90,0]) { rotate([0,0,90]) {
                    pushButtonWell(fingerButtonLength, pushButtonWellDepth+fingerRad);
                }}
            }
        }      
    }

    // thumb button well
    translate([thumbBevelWidth,caseLength-wallThick,caseHeight]) {
        rotate([0,0,180+thumbBevelAngle]) {
            rotate([0,90,0]) {
                translate([0,0,thumbKeyOffset]) {
                    rotate([0,90,0]) { rotate([0,0,-90]) {
                        //cube([50,100,200]);

                        translate([wallThick,0,0]) {
                            fingerWell(thumbWellDepth, thumbWellRadius);
                        }
                    }}

                    rotate([0,90,-90]) {
                        translate([0,wallThick,wallThick-totalThumbWellDepth]) {
                            pushButtonWell(thumbButtonLength, pushButtonWellDepth+100);

                            rotate([180,0,0]) {
                                translate([-27,-wallThick+lidThick,pushButtonBaseThick]) {
                                    cube([42,caseHeight-floorThick-lidThick,15]);
                                    cube([22,caseHeight-floorThick-lidThick,20]);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // hole for status LED
    translate([cavityWidth+wallThick-ledRimRadius-3,caseLength+1,wallThick]) {
    	    rotate(a=[90,0,0]) {
            cylinder(h=wallThick+2,r=ledDomeRadius+error, $fn=ledHoleRes);

            // note: the rim depression uses a larger error/clearance, since it doesn't need
            // to fit tightly and can't be filed down
            translate([0,0,-1+wallThick+ledRimThick]) {
                cylinder(h=10,r=ledRimRadius+clearance, $fn=20);
            }
        }  
    }

    // hole for battery charger headers
	translate([wallThick+cavityWidth-chargerHeaderWidth-error2,0,caseHeight-floorThick-chargerHeaderLength-error2]) {
        cube([chargerHeaderWidth+error2,wallThick+1,chargerHeaderLength+error2]);
    }

    // hole for SPDT Mini Power Switch
    translate([powerSwitchOffset,0,wallThick-powerSwitchWidth/2-error]) {
        cube([powerSwitchLength+error2,wallThick+1,powerSwitchWidth+error2]);
    }

    // hole for USB port
    translate([wallThick+cavityWidth-nanoLeftMargin-nanoWidth/2-nanoUsbWidth/2-clearance,0,caseHeight-floorThick-nanoHeightToUsbBase-nanoUsbHeight-clearance]) {
        cube([nanoUsbWidth+2*clearance,wallThick+1,nanoUsbHeight+clearance]);
    }
  }
}

// body
difference() {
    union() {
        basicCase();
        translate([wallThick, wallThick, 0]) {
            cylinder(h=caseHeight-floorThick+0.001, r=3);
        }
    }
    cube([caseWidth, caseLength, (caseHeight-buttonWidth)/2+0.0001]);

    for (i = [0:4]) {
        translate([wallThick+cavityWidth+10,caseLength-fingerHeightOffset-fingerRad*i*2,0]) {
            cylinder(h=caseHeight*2/3, r=1.5+error2, $fn=connectorPinRes);
        }
    }
    translate([thumbBevelWidth-3, caseLength-wallThick-6, 0]) {
        cylinder(h=caseHeight*2/3, r=1.5+error2, $fn=connectorPinRes);
    }
    translate([wallThick+2, caseLength-thumbBevelLength+3, 0]) {
        cylinder(h=caseHeight*2/3, r=1.5+error2, $fn=connectorPinRes);
    }
    translate([wallThick, wallThick, 0]) {
        cylinder(h=caseHeight*2/3, r=1.5+error2, $fn=connectorPinRes);
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

// buttons
// Note: the buttons are oriented so as to make the longest surfaces the smoothest
translate([-10,0,10]) {
    for (i = [0:3]) {
        translate([0,i*(fingerButtonLength+2),0]) { rotate([90,180,-90]) {
            button(fingerButtonLength);
        }}     
    }
}
translate([5,caseLength-35,10]) {
    rotate([90,180,-90]) {
        button(thumbButtonLength);
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
        translate([0,0,caseHeight-floorThick-nanoHeightToUsbBase-error]) {
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
