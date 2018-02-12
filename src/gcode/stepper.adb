-------------------------------------------------------------------------------
--                                                                           --
--                                   ACNC                                    --
--                                                                           --
--         Copyright (C) 2016 Fabien Chouteau (chouteau@adacore.com)         --
--                                                                           --
--                                                                           --
--    ACNC is free software: you can redistribute it and/or modify it        --
--    under the terms of the GNU General Public License as published by      --
--    the Free Software Foundation, either version 3 of the License, or      --
--    (at your option) any later version.                                    --
--                                                                           --
--    ACNC is distributed in the hope that it will be useful, but WITHOUT    --
--    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY     --
--    or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public        --
--    License for more details.                                              --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with ACNC. If not, see <http://www.gnu.org/licenses/>.           --
--                                                                           --
-------------------------------------------------------------------------------

--  with Ada.Real_Time; use Ada.Real_Time;
--  with Ada.Real_Time.Timing_Events; use Ada.Real_Time.Timing_Events;
--  with System;
with Settings; use Settings;
with Console;

package body Stepper is

   type Do_Step_Array is array (Axis_Name) of Boolean;

   procedure Dummy_Set_Step_Pin (Axis : Axis_Name) is null;
   procedure Dummy_Clear_Step_Pin  (Axis : Axis_Name) is null;
   procedure Dummy_Set_Direction_Pin (Axis : Axis_Name;
                                      Dir : Direction) is null;
   procedure Dummy_Set_Stepper_Frequency (Freq_Hz : Frequency_Value) is null;
   function Dummy_Home_Test (Axis : Axis_Name) return Boolean is (False);
   procedure Dummy_Motor_Enable  (Axis : Axis_Name; Enable : Boolean) is null;

   procedure Do_Homing;

   type Stepper_Data_Type is record
      Has_Segment : Boolean := False;
      --  Does the stepper still has a segment to execute

      Seg : Segment;

      Step_Count : Steps;
      Counter : Step_Position;

      Do_Step : Do_Step_Array := (others => False);
      --  Tells which axis has to do a step at the next iteration

      Directions       : Axis_Directions := (others => Forward);
      --  Direction of steps for eaxh axis

      Block_Steps : Step_Position;
      --  Steps for the current Motion block each axis
      Block_Event_Count : Steps;
      --  Step count for the current block

      Set_Step_Callback : Set_Step_Pin_Proc :=
        Dummy_Set_Step_Pin'Access;

      Clear_Step_Callback : Clear_Step_Pin_Proc :=
        Dummy_Clear_Step_Pin'Access;

      Set_Direction_Callback : Set_Direction_Pin_Proc :=
        Dummy_Set_Direction_Pin'Access;

      Set_Stepper_Frequency_Callback : Set_Stepper_Frequency_Proc :=
        Dummy_Set_Stepper_Frequency'Access;

      Home_Test_Callback : Home_Test_Proc :=
        Dummy_Home_Test'Access;

      Motor_Enable_Callback : Motor_Enable_Proc :=
        Dummy_Motor_Enable'Access;

      Current_Position : Step_Position := (others => 0);
      --  Keep track of the actuall position of the machine

--        Dwell_Timeout : Time := Time_First;
      --  Expiration time of the dwell command
   end record;

   St_Data : Stepper_Data_Type;

--     type Step_Timing_Event is new Timing_Event with record
--        Do_Step : Do_Step_Array;
--     end record;

   procedure Do_Step_Cycle (Do_Step : Do_Step_Array;
                            Directions  : Axis_Directions);

   -------------------
   -- Do_Step_Cycle --
   -------------------

   procedure Do_Step_Cycle (Do_Step     : Do_Step_Array;
                            Directions  : Axis_Directions)
   is
   begin
      for Axis in Axis_Name loop
         if Do_Step (Axis) then
            St_Data.Set_Direction_Callback (Axis, Directions (Axis));
         end if;
      end loop;

      Console.Wait_Milliseconds (Settings.Direction_Pulse_Delay_Ms);

      for Axis in Axis_Name loop

         if Do_Step (Axis) then
            St_Data.Set_Step_Callback (Axis);

            --  Compute new posistion
            if Directions (Axis) = Forward then
               St_Data.Current_Position (Axis) :=
                 St_Data.Current_Position (Axis) + 1;
            else
               St_Data.Current_Position (Axis) :=
                 St_Data.Current_Position (Axis) - 1;
            end if;
         end if;

         if St_Data.Current_Position (Axis) > Settings.Max_Limit (Axis) then
            Console.Print_Line (Axis'Img & " off max limit");
            raise Program_Error;
         elsif St_Data.Current_Position (Axis) < Settings.Min_Limit (Axis) then
            Console.Print_Line (Axis'Img & " off min limit");
            raise Program_Error;
--           else
--              Console.Print_Line (Axis'Img & " position: " & St_Data.Current_Position (Axis)'Img);
         end if;
      end loop;

      Console.Wait_Milliseconds (Settings.Step_Pulse_Duration_Ms);

      for Axis in Axis_Name loop
         if Do_Step (Axis) then
            St_Data.Clear_Step_Callback (Axis);
         end if;
      end loop;
   end Do_Step_Cycle;


   ----------------
   -- Step_Pulse --
   ----------------

--     protected Step_Pulse is
--        pragma Priority (System.Interrupt_Priority'Last);
--
--        procedure Start_Step_Cycle (Do_Step : Do_Step_Array;
--                                    Directions  : Axis_Directions);
--        procedure Set_Step_Pins (Event : in out Timing_Event);
--        procedure Clear_Step_Pins (Event : in out Timing_Event);
--     private
--        Set_Event   : Step_Timing_Event;
--        Clear_Event : Step_Timing_Event;
--     end Step_Pulse;
--
--     protected body Step_Pulse is
--        ----------------------
--        -- Start_Step_Cycle --
--        ----------------------
--
--        procedure Start_Step_Cycle (Do_Step     : Do_Step_Array;
--                                    Directions  : Axis_Directions)
--        is
--           Now : Time;
--           Direction_Delay : constant Time_Span :=
--             Settings.Direction_Pulse_Delay;
--
--           Step_Delay      : constant Time_Span :=
--             Settings.Step_Pulse_Duration;
--        begin
--           Step_Pulse.Set_Event.Do_Step := Do_Step;
--           Step_Pulse.Clear_Event.Do_Step := Do_Step;
--
--           --  Set direction pins now
--           for Axis in Axis_Name loop
--              --  Set_Direction pin
--              St_Data.Set_Direction_Callback (Axis, Directions (Axis));
--           end loop;
--
--           Now := Clock;
--
--           if Direction_Delay = Microseconds (0) then
--              --  Set step pins imediatly
--              Set_Step_Pins (Timing_Event (Set_Event));
--           else
--              --  Schedule the timming evnet that will set the step pins
--              Set_Handler (Set_Event, Now + Direction_Delay,
--                           Set_Step_Pins'Access);
--           end if;
--
--           if Direction_Delay = Microseconds (0)
--             and then
--               Step_Delay = Microseconds (0)
--           then
--              --  Clear step pins imediatly
--              Clear_Step_Pins (Timing_Event (Clear_Event));
--           else
--              --  Schedule the timming evnet that will clear the step pins
--              Set_Handler (Clear_Event,  Now + Direction_Delay + Step_Delay,
--                           Clear_Step_Pins'Access);
--           end if;
--        end Start_Step_Cycle;

--        -------------------
--        -- Set_Step_Pins --
--        -------------------
--
--        procedure Set_Step_Pins (Event : in out Timing_Event) is
--           Do_Step : constant Do_Step_Array :=
--             Step_Timing_Event (Timing_Event'Class (Event)).Do_Step;
--        begin
--           for Axis in Axis_Name loop
--              if Do_Step (Axis) then
--                 St_Data.Set_Step_Callback (Axis);
--              end if;
--           end loop;
--        end Set_Step_Pins;
--
--        ---------------------
--        -- Clear_Step_Pins --
--        ---------------------
--
--        procedure Clear_Step_Pins (Event : in out Timing_Event) is
--           Do_Step : constant Do_Step_Array :=
--             Step_Timing_Event (Timing_Event'Class (Event)).Do_Step;
--        begin
--           for Axis in Axis_Name loop
--              if Do_Step (Axis) then
--                 St_Data.Clear_Step_Callback (Axis);
--              end if;
--           end loop;
--        end Clear_Step_Pins;
--     end Step_Pulse;

   ---------------
   -- Do_Homing --
   ---------------

   procedure Do_Homing is
   begin
      loop
         for Axis in Axis_Name loop
            --  Only virtual homing, we go back to the position at reset
            if St_Data.Current_Position (Axis) > 0 then
               St_Data.Directions (Axis) := Backward;
               St_Data.Do_Step (Axis) := True;
            elsif St_Data.Current_Position (Axis) < 0 then
               St_Data.Do_Step (Axis) := True;
               St_Data.Directions (Axis) := Forward;
               St_Data.Do_Step (Axis) := True;
            else
               St_Data.Do_Step (Axis) := False;
            end if;
         end loop;

         Do_Step_Cycle (St_Data.Do_Step, St_Data.Directions);
         exit when (for all Do_Step of St_Data.Do_Step => Do_Step = False);
      end loop;
   end Do_Homing;

   ---------------------
   -- Execute_Segment --
   ---------------------

   procedure Execute_Segment (Seg : Segment) is
   begin

      St_Data.Seg := Seg;
      St_Data.Has_Segment := True;

      case St_Data.Seg.Kind is
         when Homing_Segment =>
            Do_Homing;
            St_Data.Has_Segment := False;
         when Dwell_Segment =>
--              St_Data.Dwell_Timeout :=
--                Clock + To_Time_Span (St_Data.Seg.Dwell_Duration);
            St_Data.Set_Stepper_Frequency_Callback
              (Settings.Dwell_Stepper_Frequency);
         when Motion_Segment =>
            --  Motion segment

            St_Data.Step_Count := St_Data.Seg.Step_Count;
            St_Data.Directions := St_Data.Seg.Directions;

            --  Set frequency for this segment
            St_Data.Set_Stepper_Frequency_Callback
              (St_Data.Seg.Frequency);

            if St_Data.Seg.New_Block then
               --  This is the first segment of a new block

               --  Prep data for bresenham algorithm
               St_Data.Counter := (others => 0);
               St_Data.Block_Steps := St_Data.Seg.Block_Steps;
               St_Data.Block_Event_Count :=
                 St_Data.Seg.Block_Event_Count;
            end if;
         when Enable_Motors_Segment =>
            Console.Print_Line ("Enable motor segment");
            for Axis in Axis_Name loop
               St_Data.Motor_Enable_Callback
                 (Axis, St_Data.Seg.Enable (Axis));
            end loop;
            St_Data.Has_Segment := False;
      end case;

      while St_Data.Has_Segment loop

         St_Data.Do_Step := (others => False);

         case St_Data.Seg.Kind is
         when Homing_Segment =>
            null;
         when Dwell_Segment =>
--              if Clock >= St_Data.Dwell_Timeout then
--                 St_Data.Has_Segment := False;
--              end if;
            raise Program_Error;
         when Motion_Segment =>
            --  Bresenham for each axis
            for Axis in Axis_Name loop

               if St_Data.Block_Steps (Axis) /= 0 then
                  St_Data.Counter (Axis) :=
                    St_Data.Counter (Axis) + St_Data.Block_Steps (Axis);
                  if St_Data.Counter (Axis) >= St_Data.Block_Event_Count then
                     St_Data.Do_Step (Axis) := True;
                     St_Data.Counter (Axis) :=
                       St_Data.Counter (Axis) - St_Data.Block_Event_Count;
                  else
                     St_Data.Do_Step (Axis) := False;
                  end if;
               end if;
            end loop;

            St_Data.Step_Count := St_Data.Step_Count - 1;
            --  Check end of segement
            if St_Data.Step_Count = 0 then
               St_Data.Has_Segment := False;
            end if;
         when Enable_Motors_Segment =>
            null;
         end case;

         Do_Step_Cycle (St_Data.Do_Step,
                        St_Data.Directions);

      end loop;
   end Execute_Segment;

   ---------------------------
   -- Set_Stepper_Callbacks --
   ---------------------------

   procedure Set_Stepper_Callbacks
     (Set_Step              : Set_Step_Pin_Proc;
      Clear_Step            : Clear_Step_Pin_Proc;
      Set_Direcetion        : Set_Direction_Pin_Proc;
      Set_Stepper_Frequency : Set_Stepper_Frequency_Proc;
      Home_Test             : Home_Test_Proc;
      Motor_Enable          : Motor_Enable_Proc)
   is
   begin
      St_Data.Set_Step_Callback := Set_Step;
      St_Data.Clear_Step_Callback := Clear_Step;
      St_Data.Set_Direction_Callback := Set_Direcetion;
      St_Data.Set_Stepper_Frequency_Callback := Set_Stepper_Frequency;
      St_Data.Home_Test_Callback := Home_Test;
      St_Data.Motor_Enable_Callback := Motor_Enable;
   end Set_Stepper_Callbacks;

end Stepper;
