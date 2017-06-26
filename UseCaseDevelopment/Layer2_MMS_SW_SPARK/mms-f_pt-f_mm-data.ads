with MMS.F_PT.Data;

with Types; use Types;

package MMS.F_PT.F_MM.Data is

   --------------------------
   -- Parameter Data Items --
   --------------------------

   --  From 6.6.2.3

   Amode_Initial_Domain_Mesh : Viability_Domain_Mesh_Type
     (1 .. 100, 1 .. 100, 1 .. 100); -- ??? bounds

   function Viability_Amode_Initial
     (M : Payload_Mass_Center;
      D : Viability_Distance_Center;
      A : Viability_Altitude_Center;
      S : Viability_Speed_Center) return Energy_Level_Type
   with Pre => M in MMS.F_PT.Data.Payload_Mass_Grid'Range
     and then D in Amode_Initial_Domain_Mesh'Range (1)
     and then A in Amode_Initial_Domain_Mesh'Range (2)
     and then S in Amode_Initial_Domain_Mesh'Range (3);

   Amode_Cruise_Domain_Mesh : Viability_Domain_Mesh_Type
     (1 .. 100, 1 .. 100, 1 .. 100); -- ??? bounds

   function Viability_Amode_Cruise
     (M : Payload_Mass_Center;
      D : Viability_Distance_Center;
      A : Viability_Altitude_Center;
      S : Viability_Speed_Center) return Energy_Level_Type
   with Pre =>  M in MMS.F_PT.Data.Payload_Mass_Grid'Range
     and then D in Amode_Cruise_Domain_Mesh'Range (1)
     and then A in Amode_Cruise_Domain_Mesh'Range (2)
     and then S in Amode_Cruise_Domain_Mesh'Range (3);

   RPmode_Initial_Domain_Mesh : Viability_Domain_Mesh_Type
     (1 .. 100, 1 .. 100, 1 .. 100); -- ??? bounds

   function Viability_RPmode_Initial
     (M : Payload_Mass_Center;
      D : Viability_Distance_Center;
      A : Viability_Altitude_Center;
      S : Viability_Speed_Center) return Energy_Level_Type
   with Pre =>  M in MMS.F_PT.Data.Payload_Mass_Grid'Range
     and then D in RPmode_Initial_Domain_Mesh'Range (1)
     and then A in RPmode_Initial_Domain_Mesh'Range (2)
     and then S in RPmode_Initial_Domain_Mesh'Range (3);

   RPmode_Cruise_Domain_Mesh : Viability_Domain_Mesh_Type
     (1 .. 100, 1 .. 100, 1 .. 100); -- ??? bounds

   function Viability_RPmode_Cruise
     (M : Payload_Mass_Center;
      D : Viability_Distance_Center;
      A : Viability_Altitude_Center;
      S : Viability_Speed_Center) return Energy_Level_Type
   with Pre =>  M in MMS.F_PT.Data.Payload_Mass_Grid'Range
     and then D in RPmode_Cruise_Domain_Mesh'Range (1)
     and then A in RPmode_Cruise_Domain_Mesh'Range (2)
     and then S in RPmode_Cruise_Domain_Mesh'Range (3);

   --  From 6.6.4 Mission termination control

   Glide_Distance_Domain_Mesh : Glide_Domain_Mesh_Type (1 .. 100); -- ??? bounds

   function Glide_Distance
     (AI : Glide_Altitude_Center) return Current_Range_Type
   with Pre => AI in Glide_Distance_Domain_Mesh'Range;

end MMS.F_PT.F_MM.Data;
