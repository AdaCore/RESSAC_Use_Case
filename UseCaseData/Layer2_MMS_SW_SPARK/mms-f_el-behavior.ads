with Types; use Types;

package MMS.F_EL.Behavior with SPARK_Mode is
   
   ------------
   -- Inputs --
   ------------

   function P return Distance_Type with Global => Private_State;
   
   function P_Dot return Speed_Type with Global => Private_State;
   
   function Q return Angle_Type with Global => Private_State;
   
   ----------------------
   -- Estimated Values --
   ----------------------

   function Q_Dot return Speed_Type with Global => Private_State;
   
   ---------------------------------------
   -- Behavioural Specification of F_EL --
   ---------------------------------------

   procedure Read_Inputs with
   --  Read values of inputs once and for all and update the current state
     Global => (In_Out => Private_State);

   procedure Write_Outputs with
   --  Compute values of outputs from the current state
     Global => (Input  => Private_State,
                Output => Output_State);

   procedure Run with
     Global => (In_Out => Private_State); 

end MMS.F_EL.Behavior;
