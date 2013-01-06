
function pythag(a, b) = sqrt(a*a + b*b);
function angle(a, b) = atan(a/b);

visualizeInternalComponents = false;

// this is the accuracy value for Shapeways' "Strong & Flexible Plastics" option
error = 0.15;

// note: AFAIK, "clearance" is a minimum gap in printed parts.  Here, however, it is used
// as a lesser/safer accuracy for gaps between the printed part and foreign objects such as
// circuit boards, which may not have been cut or measured with the same degree of precision.
clearance = 0.5;

containmentWallThick = 1;

nanoLength = 43.2;
nanoWidth = 18.0;
nanoHeightWithoutUsb = 5.4;
nanoHeightToUsbBase = 3.3;
nanoUsbWidth = 7.6;
nanoUsbHeight = 3.9;
nanoLeftMargin = 5;

chargerHeaderLength = 5.10;
chargerHeaderWidth = 2.56;

powerSwitchLength = 11.6;
powerSwitchWidth = 4.0;
powerSwitchDepth = 7.5;
powerSwitchOffset = 12;

ledDomeRadius = 2.5;
ledRimRadius = 2.9;

caseWidth = 70;
caseLength = 130;
caseHeight = 11;

wallThick = caseHeight/2;
lidThick = 1.5;
floorThick = 1.5;
lidMargin = 1;

fingerHeightOffset = 5;
fingerWellDepth = 10;
fingerRad = 15;
thumbWellDepth = wallThick;
thumbWellRadius = 30;
pushButtonWellWidth = 7;
pushButtonWellDepth = 4;
pushButtonBaseThick = 1;
pushButtonLegLength = 4;
pushButtonHeight = 5.0;
pushButtonPressDepth = 0.3;
//pushButtonRadius = 3.5/2;
pushButtonLegProtrusion = pushButtonLegLength - pushButtonBaseThick;
buttonThick = 8;
buttonWidth = pushButtonWellWidth;
buttonLength = 25;
buttonStabilizerWellWidth = 3;
buttonStabilizerRodHeight = pushButtonHeight - (pushButtonPressDepth + 0.5 + error);
totalFingerWellDepth = fingerWellDepth + buttonThick/2 + pushButtonPressDepth + pushButtonWellDepth;
totalThumbWellDepth = thumbWellDepth + buttonThick/2 + pushButtonPressDepth + pushButtonWellDepth;

buttonClearance = 0.3;

thumbBevelLength = 48;
thumbBevelWidth = 31;

thumbBevelAngle = angle(thumbBevelLength-wallThick,thumbBevelWidth-wallThick);
thumbBevelStretch = pythag(thumbBevelLength-wallThick, thumbBevelWidth-wallThick);

thumbKeyOffset = 23;

cavityWidth = caseWidth - wallThick - totalFingerWellDepth - pushButtonBaseThick;
cavityLength = caseLength - (2 * wallThick);

lidWidth = caseWidth - wallThick + lidMargin - totalFingerWellDepth + 3;
lidLength = caseLength - thumbBevelLength - wallThick + 2*lidMargin;

// button well with origin at the center of the base of the switch
// legs of the switch run along the x axis
module pushButtonWell(depth) {
    translate([-pushButtonWellWidth/2, -pushButtonWellWidth/2, 0]) {

		// square well for body of switch
        cube([pushButtonWellWidth,pushButtonWellWidth, depth]);

		// channels for legs/wires of switch
        translate([0,0,-pushButtonLegLength]) {
                translate([-1,0,0]) {
                    cube([2,2,depth+pushButtonLegLength]);
                }
                translate([pushButtonWellWidth-1,0, 0]) {
                    cube([2,2,depth+pushButtonLegLength]);
                } 
                translate([-1,pushButtonWellWidth-2, 0]) {
                    cube([2,2,depth+pushButtonLegLength]);
                } 
                translate([pushButtonWellWidth-1,pushButtonWellWidth-2, 0]) {
                    cube([2,2,depth+pushButtonLegLength]);
                }
        }		
    }

    // rectangular well for button body, and stabilizer wells
    translate([-buttonLength/2,-buttonWidth/2,pushButtonWellDepth]) {
        cube([buttonLength,buttonWidth,buttonThick+pushButtonPressDepth+10]);

        translate([0,0,-pushButtonWellDepth]) {
            cube([buttonStabilizerWellWidth, buttonWidth, pushButtonWellDepth]);
		}

        translate([buttonLength-buttonStabilizerWellWidth,0,-pushButtonWellDepth]) {
            cube([buttonStabilizerWellWidth, buttonWidth, pushButtonWellDepth]);
		}
    }
}

module fingerWell(depth, radius) {
    translate([radius - depth,0,0]) {
        cylinder(h=caseHeight,r=radius);
    }

    translate([radius - depth, 0, wallThick]) {
        cylinder(h=wallThick+.001, r1=radius-wallThick/2, r2=radius+wallThick/2);    
    }

    translate([radius - depth,0,0]) {
        cylinder(h=wallThick+.001, r2=radius-wallThick/2, r1=radius+wallThick/2);    
    }
}

// create the lid
// buffer: hyper-extends the lid, for better rendering
// shrinkage: clearance around the edges of the lid
module lid(buffer, shrinkage) {
    translate([0,0,-buffer]) {
        translate([wallThick-lidMargin+shrinkage,wallThick-lidMargin+shrinkage,0]) {
            cube([lidWidth,lidLength,lidThick+buffer]);
        }
        translate([thumbBevelWidth,wallThick-lidMargin+shrinkage,0]) {
            cube([lidWidth-thumbBevelWidth+wallThick,cavityLength+(lidMargin-shrinkage)*2,lidThick+buffer]);
        }

        // note: currently no "shrinkage" on this edge
        translate([thumbBevelWidth,caseLength-wallThick,caseHeight]) {
            rotate([0,0,180+thumbBevelAngle]) {
                rotate([0,90,0]) {
                    translate([0,0,thumbKeyOffset]) {
                        rotate([0,90,-90]) {
                            translate([0,wallThick,wallThick-totalThumbWellDepth]) {
                                rotate([180,0,0]) {
                                    translate([-27,-wallThick,pushButtonBaseThick]) {
                                        translate([0,0,-6]) {
                                            cube([47,lidThick+buffer,20]);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

module button() {
    difference() {
        union() {
                    translate([buttonClearance,buttonWidth/2,buttonWidth/2]) {
                        rotate([0,90,0]) { cylinder(h=buttonLength-2*buttonClearance,r=buttonWidth/2-buttonClearance); }
                    }

                    translate([buttonClearance,buttonClearance,buttonWidth/2]) {
                        cube([buttonLength-2*buttonClearance,buttonWidth-2*buttonClearance,buttonThick-buttonWidth/2]);
                    }

                    translate([buttonClearance,buttonClearance,buttonThick]) {
                        cube([buttonStabilizerWellWidth-2*buttonClearance, buttonWidth-2*buttonClearance, buttonStabilizerRodHeight]);
		            }

                    translate([buttonLength-buttonStabilizerWellWidth+buttonClearance,buttonClearance,buttonThick]) {
                        cube([buttonStabilizerWellWidth-2*buttonClearance, buttonWidth-2*buttonClearance, buttonStabilizerRodHeight]);
		            }
        }

        translate([buttonClearance+1,0,0]) { cube([1,buttonWidth,2]); }
        translate([buttonLength-buttonClearance-2,0,0]) { cube([1,buttonWidth,2]); }

        //translate([0,buttonWidth/2,buttonWidth/2]) {
        //    rotate([0,90,0]) { cylinder(h=buttonLength+1,r=buttonWidth/3); }
        //}

        //translate([buttonLength/2,buttonWidth/2,buttonThick-0.5]) {
        //    cylinder(h=1,r=pushButtonRadius);
        //}
    }
}

module screwHoles() {
    translate([0,0,-1]) {
        translate([15,caseLength-thumbBevelLength-wallThick+9,0]) { cylinder(h=caseHeight+2,r=1.5); }

        translate([wallThick+cavityWidth+2.5,caseLength-fingerHeightOffset-fingerRad*2,0]) { cylinder(h=caseHeight+2,r=1.5); }
        translate([wallThick+cavityWidth+2.5,caseLength-fingerHeightOffset-fingerRad*4,0]) { cylinder(h=caseHeight+2,r=1.5); }
        translate([wallThick+cavityWidth+2.5,caseLength-fingerHeightOffset-fingerRad*6,0]) { cylinder(h=caseHeight+2,r=1.5); }
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
            sphere(wallThick);
            rotate([-90,0,0]) {
                cylinder(h=caseLength-thumbBevelLength-wallThick, r=wallThick);
            }
            rotate([0,90,0]) {
                cylinder(h=caseWidth-caseHeight, r=wallThick);
            }
        }
        translate([caseWidth-wallThick,wallThick,wallThick]) {
            sphere(wallThick);
            rotate([-90,0,0]) {
                cylinder(h=caseLength-caseHeight, r=wallThick);
            }
        }
        translate([caseWidth-wallThick,caseLength-wallThick,wallThick]) {
            sphere(wallThick);
        }

		// thumb bevel
		translate([wallThick,caseLength-thumbBevelLength,wallThick]) {
            sphere(wallThick);
        }
        translate([thumbBevelWidth,caseLength-wallThick,wallThick]) {
            sphere(wallThick);
            rotate([0,90,0]) {
                cylinder(h=caseWidth-thumbBevelWidth-wallThick, r=wallThick);
            }
            rotate([0,0,180+thumbBevelAngle]) {
                rotate([0,90,0]) { 
                    cylinder(h=thumbBevelStretch, r=wallThick);
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
    translate([thumbBevelWidth+2,wallThick,lidThick]) {
        cube([cavityWidth-thumbBevelWidth+wallThick-2,cavityLength,caseHeight-floorThick-lidThick]);
    }

    /*
    // depression for lid
    lid(1,0);
    */

    screwHoles();

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
                    pushButtonWell(pushButtonWellDepth+fingerRad);
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
                            pushButtonWell(pushButtonWellDepth+100);

                            rotate([180,0,0]) {
                                translate([-27,-wallThick+lidThick,pushButtonBaseThick]) {
                                    cube([39,caseHeight-floorThick-lidThick,15]);
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
            cylinder(h=wallThick+2,r=ledDomeRadius+error);

            // note: the rim depression uses a larger error/clearance, since it doesn't need
            // to fit tightly and can't be filed down
            translate([0,0,wallThick]) {
                cylinder(h=10+1,r=ledRimRadius+clearance);
            }
        }  
    }

    // hole for battery charger headers
	translate([wallThick,0,caseHeight-floorThick-chargerHeaderLength-2*error]) {
        cube([chargerHeaderWidth+2*error,wallThick,chargerHeaderLength+2*error]);
    }

    // hole for SPDT Mini Power Switch
    translate([powerSwitchOffset,0,wallThick-powerSwitchWidth/2-error]) {
        cube([powerSwitchLength+2*error,wallThick,powerSwitchWidth+2*error]);
    }

    // hole for USB port
    translate([wallThick+cavityWidth-nanoLeftMargin-nanoWidth/2-nanoUsbWidth/2-clearance,0,caseHeight-floorThick-nanoHeightToUsbBase-nanoUsbHeight-clearance]) {
        cube([nanoUsbWidth+2*clearance,wallThick,nanoUsbHeight+clearance]);
    }
    // trim off the thin remainder above the USB port which would be problematic to print,
    // and make it harder to get the Nano into its socket anyway
    translate([wallThick+cavityWidth-nanoLeftMargin-nanoWidth/2-nanoUsbWidth/2-clearance,wallThick-lidMargin,0]) {
        cube([nanoUsbWidth+2*clearance,lidMargin,2]);
    }
  }
}

difference() {
    basicCase();
    cube([caseWidth, caseLength, (caseHeight-buttonWidth)/2+0.0001]);
}

translate([caseWidth + 10, 0, 0]) {
    difference() {
        basicCase();
        translate([0,0,(caseHeight-buttonWidth)/2]) {
            cube([caseWidth, caseLength, caseHeight]);
        }
    }
}

/*
// lid
translate([0,0,caseHeight + 2]) {
    difference() {
       // the gap of 0.4 was determined by taking into account the 0.15mm accuracy of
       // the device and the 0.15% error by longest axis of the material, keeping in mind
	   // that there are always a pair of gaps across the lid from each other.
       // 0.4mm is actually larger than necessary in the horizontal (narrower) dimension.
       lid(0,0.4);

       screwHoles();
    }
}
*/

// buttons
// Note: the buttons are oriented so as to make the longest surfaces the most smooth
translate([-5,0,10]) {
    for (i = [0:4]) {
        translate([0,i*(buttonLength+2),0]) { rotate([90,180,-90]) {
            //cube([10,20,40]);

            button();
        }}     
    }
}

// electronic components
translate([wallThick,wallThick,0]) {
    // power switch
    translate([powerSwitchOffset-wallThick,0,wallThick-powerSwitchWidth/2-error]) {
        translate([-containmentWallThick,0,0]) {
            cube([containmentWallThick,powerSwitchDepth-wallThick+error+containmentWallThick,caseHeight-floorThick-wallThick+powerSwitchWidth/2+error]);
        }
        translate([powerSwitchLength+2*error,0,0]) {
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
        translate([0,0,-10]) {
            cube([nanoWidth,nanoLength,nanoHeightWithoutUsb]);
        }

        // Surface Transducer - Small
        translate([thumbBevelWidth-wallThick+4,caseLength-2*wallThick-21.4-4,-10]) {
           cube([13.8,21.4,7.9]);
        }
        // Bluetooth Modem - BlueSMiRF Silver -- 42.0 x 16.0 x 3.9
        translate([1,31,-10]) { cube([16,42,3.9]); }
        // Polymer Lithium Ion Battery - 110mAh
        translate([-40,3,-10]) { cube([12,28,5.7]); }
        // Polymer Lithium Ion Battery - 400mAh
	    translate([18,47,-10]) { cube([25,35,5]); }
        // Triple Axis Accelerometer & Gyro Breakout - MPU-6050
        translate([1,4,-10]) { cube([15.5, 25.7, 2.5]); }
    }
}
