!deck Lebedev_Data
!***begin prologue     Lebedev_Data
!***date written       140601   (yymmdd)
!***revision date      yymmdd   (yymmdd)
!***keywords           Lebedev_Data
!***author             schneider, barry (nsf)
!***source             m6200
!***purpose            Lebedev_Data for the numerical grid generation code
!***description  
!***             
!***             
!***references
!***routines called    
!***end prologue       Lebedev_Data
!********************************************************************************
!********************************************************************************
                        MODULE Lebedev_Data
!----------------------------------------------------------------------------------------------------------
  INTEGER, PARAMETER                      :: order_max = 5810
  INTEGER, PARAMETER                      :: rule_max  = 65
  INTEGER, PARAMETER                      :: mmax = ( ( rule_max * 2 + 3 ) * ( rule_max * 2 + 3 ) / 3 )
  INTEGER                                 :: order
  INTEGER                                 :: grid_rule
  INTEGER                                 :: precision
  INTEGER, PARAMETER, DIMENSION(rule_max) :: Rule_Logic_Table =                                        &
                                            [ 1,    1,    1,    1,    1,    1,    1,    1,    1,    1, &
                                              1,    1,    1,    1,    1,    0,    1,    0,    0,    1, &
                                              0,    0,    1,    0,    0,    1,    0,    0,    1,    0, &
                                              0,    1,    0,    0,    1,    0,    0,    1,    0,    0, &
                                              1,    0,    0,    1,    0,    0,    1,    0,    0,    1, &
                                              0,    0,    1,    0,    0,    1,    0,    0,    1,    0, &
                                              0,    1,    0,    0,    1 ]
!
  INTEGER, PARAMETER, DIMENSION(rule_max) :: Rule_Order_Table =                                        &
                                          [ 6,   14,   26,   38,   50,   74,   86,  110,  146,  170,   &
                                          194,  230,  266,  302,  350,  386,  434,  482,  530,  590,   &
                                          650,  698,  770,  830,  890,  974, 1046, 1118, 1202, 1274,   &
                                          1358, 1454, 1538, 1622, 1730, 1814, 1910, 2030, 2126, 2222,   &
                                          2354, 2450, 2558, 2702, 2810, 2930, 3074, 3182, 3314, 3470,   &
                                          3590, 3722, 3890, 4010, 4154, 4334, 4466, 4610, 4802, 4934,   &
                                          5090, 5294, 5438, 5606, 5810 ]
!
  INTEGER, PARAMETER, DIMENSION(rule_max) :: Precision_Table =                                         &
                                          [ 3,   5,   7,   9,  11,  13,  15,  17,  19,  21,            &
                                           23,  25,  27,  29,  31,  33,  35,  37,  39,  41,            &
                                           43,  45,  47,  49,  51,  53,  55,  57,  59,  61,            &
                                           63,  65,  67,  69,  71,  73,  75,  77,  79,  81,            &
                                           83,  85,  87,  89,  91,  93,  95,  97,  99, 101,            &
                                          103, 105, 107, 109, 111, 113, 115, 117, 119, 121,            &
                                          123, 125, 127, 129, 131 ]
!---------------------------------------------------------------------------------------------------------
!********************************************************************************
!********************************************************************************
  END MODULE Lebedev_DATA
!********************************************************************************
!********************************************************************************
