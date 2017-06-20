package MMS.F_PT.F_MM.Data is

   --------------------------
   -- Parameter Data Items --
   --------------------------

   --  From 6.6.2.3

   function Viability_Amode_initial
     (Payload_Mass    : Payload_Mass_Type;
      Distance        : Current_Range_Type;
      Navigation_Mode : Navigation_Mode_Type;
      Altitude        : Current_Altitude_Type;
      Speed           : Current_Speed_Type)
      return Energy_Level_Type;

   function Viability_Amode_cruise
     (Payload_Mass    : Payload_Mass_Type;
      Distance        : Current_Range_Type;
      Navigation_Mode : Navigation_Mode_Type;
      Altitude        : Current_Altitude_Type;
      Speed           : Current_Speed_Type)
      return Energy_Level_Type;

   function Viability_RPmode_initial
     (Payload_Mass    : Payload_Mass_Type;
      Distance        : Current_Range_Type;
      Navigation_Mode : Navigation_Mode_Type;
      Altitude        : Current_Altitude_Type;
      Speed           : Current_Speed_Type)
      return Energy_Level_Type;

   function Viability_RPmode_cruise
     (Payload_Mass    : Payload_Mass_Type;
      Distance        : Current_Range_Type;
      Navigation_Mode : Navigation_Mode_Type;
      Altitude        : Current_Altitude_Type;
      Speed           : Current_Speed_Type)
      return Energy_Level_Type;

   --  From 6.6.4 Mission termination control

   function Glide_Distance
     (Altitude : Current_Altitude_Type) return Current_Range_Type;

end MMS.F_PT.F_MM.Data;
