with Types; use Types;

package MMS.F_PT.F_MM with
  SPARK_Mode,
  Abstract_State => (Private_State, Output_State, Input_State)
is
   pragma Elaborate_Body (MMS.F_PT.F_MM);

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

   type Mission_Profile_Type is record
      Mass     : Payload_Mass_Type;
      Distance : Current_Range_Type;
      Altitude : Current_Altitude_Type;
      Speed    : Current_Speed_Type;
   end record;

   type Center_Mission_Profile_Type is record
      M : Payload_Mass_Center;
      D : Viability_Distance_Center;
      A : Viability_Altitude_Center;
      S : Viability_Speed_Center;
   end record;

   type Mission_Profile_Distance_Type is new Natural;

   type Neighbour_Mission_Profile_Type is record
      Mission_Profile : Center_Mission_Profile_Type;
      Distance        : Mission_Profile_Distance_Type;
   end record;

   type Neighbour_Mission_Profile_Array_Type is array (Positive range 1 .. 16)
     of Neighbour_Mission_Profile_Type;

end MMS.F_PT.F_MM;
