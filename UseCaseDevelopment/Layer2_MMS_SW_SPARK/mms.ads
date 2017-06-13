--  The structure of the MMS system has been created from the System
--  Requirements document following the pattern below:
--   * Components are packages.
--   * Sub-Components are child packages.
--   * Inputs and outputs are functions stored in child packages Comp.Input and
--     Comp.Output.
--   * Connections between inputs and outputs of various components are done
--     using renamings (or eventually expression functions when necessary).
--     Consistency of the component architecture is ensured by following the
--     rules below:
--      - An input package Comp.Subcomp.Input can only reference outputs of
--        siblings Comp.*.Output or inputs of parent Comp.Input.
--      - An output package Comp.Output can only reference its own inputs
--        Comp.Input or outputs of its children Comp.Subcomp.Output.
--      - In subcomponents, inputs and outputs are grouped within sections
--        with a header specifying to which component(s) they are linked.

package MMS is

   ----------------------------------
   -- Types for inputs and outputs --
   ----------------------------------

   type Distance_Input_Type is range 1 .. 100; -- in n.m
   
   type Speed_Input_Type is range 1 .. 250; -- in k.t
   
   type Altitude_Input_Type is range -500 .. 3000; -- in ft
   
   type Navigation_Parameters_Type is record
      Distance : Distance_Input_Type;
      Speed    : Speed_Input_Type;
      Altitude : Altitude_Input_Type;
   end record;
   
   type Navigation_Mode_Type is (RP, A);
   
   type Navigation_Option_Type is (SPEED, ALTITUDE, ENERGY);
   
   type Bay_Switch_Type is (OPEN, CLOSED);
   
   type Payload_Mass_Type is delta 0.1 range 1.0 .. 5.0; -- ??? in kg

   type Distance_Type is new Float; -- in n.m
   
   type Speed_Type is new Float; -- in k.t
   
   type Altitude_Type is new Float; -- in ft
   
   type Rotactor_Type is range 1 .. 9;
   
   type CP_Switches_Type is record
      Power      : Boolean;
      Mode       : Navigation_Mode_Type;
      Bay        : Bay_Switch_Type;
      Start      : Boolean;
      Rotactor_1 : Rotactor_Type;
      Rotactor_2 : Rotactor_Type;
   end record;

   type Source_Type is range 1 .. 100;
   
   type CP_Displays_Type is record
      Ready            : Boolean;
      Cancelled        : Boolean;
      Complete         : Boolean;
      Aborted          : Boolean; -- ???
      Primary_Source   : Source_Type;
      Secondary_Source : Source_Type;
   end record;
   
   type Source_Capacity_Type is range 1 .. 99_999;
   
   type Propulsion_Energy_Type is record
      Primary_Source_Capacity   : Source_Capacity_Type;
      Secondary_Source_Capacity : Source_Capacity_Type;
   end record;
   
   type Torque_Type is delta 0.0001 range -10.0E6 .. 10.0E6; -- ???
   
   type Current_Range_Type is range 1 .. 1_000_000; -- in meters
   
   type Current_Speed_Type is range 1 .. 500;  -- in km/h
   
   type Current_Altitude_Type is range -200 .. 1_000; -- in meters
   
   type Estimated_Total_Mass_Type is delta 0.1 range 5.0 .. 10.0; -- in kg ???
   
   type Energy_Level_Type is range 0 .. 500; -- in kj
   
   type Operating_Point_Type is record
      Altitude : Current_Altitude_Type; -- ??? which altitude type
      Speed    : Current_Speed_Type; --  ??? which speed type
   end record;
end MMS;
