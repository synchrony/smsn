
function pythag(a, b) = sqrt(a*a + b*b);
function angle(a, b) = atan(a/b);

caseWidth = 70;
caseLength = 130;
caseHeight = 11;

wallThick = caseHeight/2;
lidThick = 1.5;
floorThick = 1.5;
lidMargin = 1.5;

fingerHeightOffset = 5;
fingerWellDepth = 10;
fingerRad = 15;
thumbWellDepth = wallThick;
thumbWellRadius = 30;
pushButtonWellWidth = 7;
pushButtonWellDepth = 4;
pushButtonBaseThick = 1;
pushButtonLegLength = 4;
pushButtonRadius = 3.5/2;
pushButtonLegProtrusion = pushButtonLegLength - pushButtonBaseThick;
buttonThick = 8;
buttonPressDepth = 1;
buttonWidth = pushButtonWellWidth;
buttonLength = 25;
buttonStabilizerWellWidth = 3;
buttonStabilizerRodHeight = pushButtonWellDepth - 1;
totalFingerWellDepth = fingerWellDepth + buttonThick/2 + buttonPressDepth + pushButtonWellDepth;
totalThumbWellDepth = thumbWellDepth + buttonThick/2 + buttonPressDepth + pushButtonWellDepth;

ledDomeRadius = 2.6;
ledRimRadius = 3.1;

thumbBevelLength = 50;
thumbBevelWidth = 35;

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
        cube([buttonLength,buttonWidth,buttonThick+buttonPressDepth+10]);

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

module screwHoles() {
    translate([-1,0,0]) {
        translate([17,caseLength-thumbBevelLength-wallThick+9,0]) { cylinder(h=caseHeight+2,r=1.5); }

        translate([wallThick+cavityWidth+3.5,caseLength-fingerHeightOffset-fingerRad*2,0]) { cylinder(h=caseHeight+2,r=1.5); }
        translate([wallThick+cavityWidth+3.5,caseLength-fingerHeightOffset-fingerRad*4,0]) { cylinder(h=caseHeight+2,r=1.5); }
        translate([wallThick+cavityWidth+3.5,caseLength-fingerHeightOffset-fingerRad*6,0]) { cylinder(h=caseHeight+2,r=1.5); }
    }
}

difference() {
    // basic shape of the case
    union() {
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

    // inner compartment
    translate([wallThick,wallThick,0]) {
        cube([cavityWidth,caseLength-thumbBevelLength-wallThick,caseHeight-floorThick]);
    }
    translate([thumbBevelWidth+2,wallThick,0]) {
        cube([cavityWidth-thumbBevelWidth+wallThick-2,cavityLength,caseHeight-floorThick]);
    }

    // depression for lid
    lid(1,0);

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
                                translate([-27,-wallThick-1,pushButtonBaseThick]) {
                                    cube([39,caseHeight-floorThick+1,15]);
                                    cube([22,caseHeight-floorThick+1,20]);
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
            cylinder(h=wallThick+2,r=ledDomeRadius);

            translate([0,0,wallThick]) {
                cylinder(h=10+1,r=ledRimRadius);
            }
        }  
    }

    // hole for battery charger headers
	translate([wallThick+cavityWidth-3,-1,wallThick]) {
        translate([-2,0,-3.5]) { cube([4,wallThick+2,7]); }
    }

    // hole for USB port
    translate([31,0,wallThick-2.5]) {
        cube([8,wallThick,5]);
    }

    // socket for SPDT slide switch
    translate([10,0,wallThick-2.25]) {
        cube([12,wallThick,4.5]);
    }
}

// lid
translate([0,0,50]) {
    difference() {
       lid(0,0.5);

       screwHoles();
    }
}

// buttons
translate([caseWidth+30,wallThick,wallThick]) {
    for (i = [0:4]) {
        translate([0,i*2*fingerRad,0]) { rotate([0,-90,0]) { rotate([0,0,90]) {
            //cube([10,20,40]);

            difference() {
                union() {
                    translate([0,buttonWidth/2,buttonWidth/2]) {
                        rotate([0,90,0]) { cylinder(h=buttonLength,r=buttonWidth/2); }
                    }

                    translate([0,0,buttonWidth/2]) {
                        cube([buttonLength,buttonWidth,buttonThick-buttonWidth/2]);
                    }

                    translate([0,0,buttonThick]) {
                        cube([buttonStabilizerWellWidth, buttonWidth, buttonStabilizerRodHeight]);
		            }

                    translate([buttonLength-buttonStabilizerWellWidth,0,buttonThick]) {
                        cube([buttonStabilizerWellWidth, buttonWidth, buttonStabilizerRodHeight]);
		            }
                }

                translate([buttonLength/2,buttonWidth/2,buttonThick-1]) {
                    cylinder(h=2,r=pushButtonRadius);
                }
            }
        }}}     
    }
}

// electronic components (for visualization only; comment out before printing!)
translate([wallThick,wallThick,lidThick]) {
    // Arduino Nano v3.0
    translate([22,1,0]) { cube([18.5,43.2,1]); }
    // Bluetooth Modem - BlueSMiRF Silver
    translate([1,1,0]) { cube([16.6,45,3.9]); }
    // Surface Transducer - Small
    translate([1,50,0]) { cube([14.5,21.5,7.9]); }
    // Polymer Lithium Ion Battery - 110mAh
    translate([-40,3,0]) { cube([12,28,5.7]); }
    // Polymer Lithium Ion Battery - 400mAh
	translate([18,47,0]) { cube([25,35,5]); }

    // TODO: MPU-6050 breakout board
}
    



