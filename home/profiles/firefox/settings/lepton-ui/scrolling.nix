""
# ** Scrolling Settings *******************************************************
# == Only Sharpen Scrolling ===================================================
#         Pref                                             Value      Original
/*
mousewheel.min_line_scroll_amount =                 10; #        5
general.smoothScroll.mouseWheel.durationMinMS =     80; #       50
general.smoothScroll.currentVelocityWeighting = "0.15"; #   "0.25"
general.smoothScroll.stopDecelerationWeighting = "0.6"; #    "0.4"
*/
# == Smooth Scrolling ==========================================================
# ** Scrolling Options ********************************************************
# based on natural smooth scrolling v2 by aveyo
# this preset will reset couple extra variables for consistency
#         Pref                                              Value                 Original
/*
apz.allow_zooming =                               true;            /#     true
apz.force_disable_desktop_zooming_scrollbars =   false;            /#    false
apz.paint_skipping.enabled =                      true;            /#     true
apz.windows.use_direct_manipulation =             true;            /#     true
dom.event.wheel-deltaMode-lines.always-disabled = true;            /#    false
general.smoothScroll.currentVelocityWeighting = "0.12";            /#   "0.25" <- 1. If scroll too slow, set to "0.15"
general.smoothScroll.durationToIntervalRatio =    1000;            /#      200
general.smoothScroll.lines.durationMaxMS =         100;            /#      150
general.smoothScroll.lines.durationMinMS =           0;            /#      150
general.smoothScroll.mouseWheel.durationMaxMS =    100;            /#      200
general.smoothScroll.mouseWheel.durationMinMS =      0;            /#       50
general.smoothScroll.mouseWheel.migrationPercent = 100;            /#      100
general.smoothScroll.msdPhysics.continuousMotionMaxDeltaMS = 12;   /#      120
general.smoothScroll.msdPhysics.enabled =                  true;   /#    false
general.smoothScroll.msdPhysics.motionBeginSpringConstant = 200;   /#     1250
general.smoothScroll.msdPhysics.regularSpringConstant =     200;   /#     1000
general.smoothScroll.msdPhysics.slowdownMinDeltaMS =         10;   /#       12
general.smoothScroll.msdPhysics.slowdownMinDeltaRatio =  "1.20";   /#    "1.3"
general.smoothScroll.msdPhysics.slowdownSpringConstant =   1000;   /#     2000
general.smoothScroll.other.durationMaxMS =         100;            /#      150
general.smoothScroll.other.durationMinMS =           0;            /#      150
general.smoothScroll.pages.durationMaxMS =         100;            /#      150
general.smoothScroll.pages.durationMinMS =           0;            /#      150
general.smoothScroll.pixels.durationMaxMS =        100;            /#      150
general.smoothScroll.pixels.durationMinMS =          0;            /#      150
general.smoothScroll.scrollbars.durationMaxMS =    100;            /#      150
general.smoothScroll.scrollbars.durationMinMS =      0;            /#      150
general.smoothScroll.stopDecelerationWeighting = "0.6";            /#    "0.4"
layers.async-pan-zoom.enabled =                   true;            /#     true
layout.css.scroll-behavior.spring-constant =   "250.0";            /#   "250.0"
mousewheel.acceleration.factor =                     3;            /#       10
mousewheel.acceleration.start =                     -1;            /#       -1
mousewheel.default.delta_multiplier_x =            100;            /#      100
mousewheel.default.delta_multiplier_y =            100;            /#      100
mousewheel.default.delta_multiplier_z =            100;            /#      100
mousewheel.min_line_scroll_amount =                  0;            /#        5
mousewheel.system_scroll_override.enabled =       true;            /#     true <- 2. If scroll too fast, set to false
mousewheel.system_scroll_override_on_root_content.enabled = false; /#     true
mousewheel.transaction.timeout =                  1500;            /#     1500
toolkit.scrollbox.horizontalScrollDistance =         4;            /#        5
toolkit.scrollbox.verticalScrollDistance =           3;            /#        3
*/

