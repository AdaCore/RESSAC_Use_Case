with Types; use Types;

package MMS.F_PT is
   
   type Current_Range_Type is range 1 .. 1_000_000; -- in meters
   
   type Current_Speed_Type is range 1 .. 500;  -- in km/h
   
   type Current_Altitude_Type is range -200 .. 1_000; -- in meters
   
   type Estimated_Total_Mass_Type is delta 0.1 range 5.0 .. 10.0; -- in kg ???
   
   type Energy_Level_Type is range 0 .. 500; -- in kj
   
   type Operating_Point_Type is record
      Altitude : Current_Altitude_Type; -- ??? which altitude type
      Speed    : Current_Speed_Type; --  ??? which speed type
   end record;

   type Payload_Mass_Center is new Positive;

   type Payload_Mass_Grid_Type is array (Payload_Mass_Center range <>)
     of Payload_Mass_Type;

end MMS.F_PT;
