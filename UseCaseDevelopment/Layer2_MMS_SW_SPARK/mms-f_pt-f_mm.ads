with Types; use Types;

package MMS.F_PT.F_MM is

   type Viability_Cell_Center_Type is record
      Distance     : Current_Range_Type;
      Altitude     : Current_Altitude_Type;
      Speed        : Current_Speed_Type;
   end record;

   type Viability_Distance_Center is new Positive;
   type Viability_Altitude_Center is new Positive;
   type Viability_Speed_Center is new Positive;

   type Viability_Domain_Mesh_Type is array
     (Viability_Distance_Center range <>,
      Viability_Altitude_Center range <>,
      Viability_Speed_Center range <>)
     of Viability_Cell_Center_Type;

   type Glide_Altitude_Center is new Positive;

   type Glide_Domain_Mesh_Type is array
     (Glide_Altitude_Center range <>) of Current_Altitude_Type;

end MMS.F_PT.F_MM;
