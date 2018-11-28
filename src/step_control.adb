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

--  with Ada.Synchronous_Task_Control;
--  with Ada.Real_Time; use Ada.Real_Time;
--  with Stepper;
--  with System;
with Settings;
with Console;
with Stepper;
with Gcode; use Gcode;
with HAL.GPIO;

package body Step_Control is

--     Task_Sync   : Ada.Synchronous_Task_Control.Suspension_Object;
   Current_Dir : Axis_Directions := (others => Forward);
--     Task_Period : Time_Span := Milliseconds (500);

   procedure Set_Step_Direction (Axis : Axis_Name;
                                 Dir : Direction);
   procedure Clear_Step_Pin (Axis : Axis_Name);
   procedure Set_Step_Pin (Axis : Axis_Name);
   procedure Set_Stepper_Frequency (Freq_Hz : Frequency_Value);
   function Home_Test (Axis : Axis_Name) return Boolean;
   procedure Motor_Enable (Axis : Axis_Name;
                           Enable : Boolean);

   Z_Count : Integer := 0;

   procedure Enable_Servos;
   procedure Disable_Servos;

   -------------
   -- Set_Pen --
   -------------

   procedure Set_Pen (Pos : Pen_Position) is
   begin
      Set_Compare (PWM,
                   Pen_Cmp_ID,
                   (case Pos is
                       when High => Pen_High_Duty,
                       when Low  => Pen_Low_Duty));

      Enable_Servos;
      Console.Wait_Milliseconds (500);
      Disable_Servos;
   end Set_Pen;

   ----------------
   -- Set_Eraser --
   ----------------

   procedure Set_Eraser (Pos : Eraser_Position) is
   begin
      Set_Compare (PWM,
                   Eraser_Cmp_ID,
                   (case Pos is
                       when High => Eraser_High_Duty,
                       when Low  => Eraser_Low_Duty));
      Enable_Servos;
      Console.Wait_Milliseconds (500);
      Disable_Servos;
   end Set_Eraser;

   -------------------
   -- Enable_Servos --
   -------------------

   procedure Enable_Servos is
   begin
      Eraser_IO.Set_IO_Function (IOF1);
      Pen_IO.Set_IO_Function (IOF1);
   end Enable_Servos;

   --------------------
   -- Disable_Servos --
   --------------------

   procedure Disable_Servos is
   begin
      Eraser_IO.Set_IO_Function (Disabled);
      Pen_IO.Set_IO_Function (Disabled);
   end Disable_Servos;

   ---------------
   -- Initalize --
   ---------------

   procedure Initalize is
   begin

      -- Pen --
      --        Pen_IO.Set_IO_Function (IOF1);
      Pen_IO.Set_IO_Function (Disabled);
      Pen_IO.Set_Mode (HAL.GPIO.Output);
      Pen_IO.Set;
      Pen_IO.Invert;

      -- Eraser --
      --        Eraser_IO.Set_IO_Function (IOF1);
      Eraser_IO.Set_Mode (HAL.GPIO.Output);
      Eraser_IO.Set_IO_Function (Disabled);
      Eraser_IO.Set;
      Eraser_IO.Invert;

      Disable_Servos;

      -- PWM --

      Configure (This          => PWM,
                 Scale         => PWM_Scale,
                 Sticky        => False,
                 Reset_To_Zero => True,
                 Deglitch      => False);

      Set_Compare (PWM, 0, PWM_Period);
      Set_Compare (PWM, Pen_Cmp_ID, Pen_High_Duty);
      Set_Compare (PWM, Eraser_Cmp_ID, Eraser_High_Duty);

      Set_Count (PWM, 0);
      Enable_Continous (PWM);
      -- Steppers --
      Enable_Motors_IO.Set_Mode (HAL.GPIO.Output);

      Step_Motor_X.Set_Mode (HAL.GPIO.Output);
      Dir_Motor_X.Set_Mode (HAL.GPIO.Output);
      Step_Motor_Y.Set_Mode (HAL.GPIO.Output);
      Dir_Motor_Y.Set_Mode (HAL.GPIO.Output);


      Set_Stepper_Frequency (Settings.Idle_Stepper_Frequency);
      Stepper.Set_Stepper_Callbacks
        (Set_Step              => Set_Step_Pin'Access,
         Clear_Step            => Clear_Step_Pin'Access,
         Set_Direcetion        => Set_Step_Direction'Access,
         Set_Stepper_Frequency => Set_Stepper_Frequency'Access,
         Home_Test             => Home_Test'Access,
         Motor_Enable          => Motor_Enable'Access);

   end Initalize;

   ------------------------
   -- Set_Step_Direction --
   ------------------------

   procedure Set_Step_Direction (Axis : Axis_Name;
                                 Dir : Direction)
   is
   begin
      Current_Dir (Axis) := Dir;
      case Axis is
         when X_Axis =>
            case Dir is
               when Forward => Dir_Motor_X.Clear;
               when Backward => Dir_Motor_X.Set;
            end case;
         when Y_Axis =>
            case Dir is
               when Forward => Dir_Motor_Y.Clear;
               when Backward => Dir_Motor_Y.Set;
            end case;
         when others =>
            null;
      end case;
   end Set_Step_Direction;

   --------------------
   -- Clear_Step_Pin --
   --------------------

   procedure Clear_Step_Pin (Axis : Axis_Name) is
   begin
      case Axis is
         when X_Axis =>
            Step_Motor_X.Clear;
         when Y_Axis =>
            Step_Motor_Y.Clear;
         when Z_Axis =>
            null;
      end case;
   end Clear_Step_Pin;

   ------------------
   -- Set_Step_Pin --
   ------------------

   procedure Set_Step_Pin (Axis : Axis_Name) is
   begin
      case Axis is
         when X_Axis =>
            Step_Motor_X.Set;
         when Y_Axis =>
            Step_Motor_Y.Set;
         when Z_Axis =>
            null;
         if Current_Dir (Axis) = Forward then
            if Z_Count = 0 then
                  Set_Pen (High);
            end if;
            Z_Count := Z_Count + 1;
         else
            if Z_Count = 0 then
                  Set_Pen (Low);
            end if;
            Z_Count := Z_Count - 1;
         end if;
      end case;
   end Set_Step_Pin;

   ---------------------------
   -- Set_Stepper_Frequency --
   ---------------------------

   procedure Set_Stepper_Frequency (Freq_Hz : Frequency_Value) is
   begin
      --        Task_Period := To_Time_Span (1.0 / Freq_Hz);
      null;
   end Set_Stepper_Frequency;

   ---------------
   -- Home_Test --
   ---------------

   function Home_Test (Axis : Axis_Name) return Boolean is
      pragma Unreferenced (Axis);
   begin
      return False;
--        return Set (Home_GPIO (Axis)) = Home_Switch_Polarity (Axis);
   end Home_Test;

   ------------------
   -- Motor_Enable --
   ------------------

   procedure Motor_Enable (Axis : Axis_Name;
                           Enable : Boolean) is
   begin
      Console.Print_Line ("Motor_Enable (" & Axis'Img & ", Enable =>" & Enable'Img & ")");
      if Axis /= Z_Axis then
         if Enable then
            Enable_Motors_IO.Clear;
         else
            Enable_Motors_IO.Set;
         end if;
      end if;
   end Motor_Enable;

   ---------------
   -- Step_Task --
   ---------------

--     task Step_Task is
--        pragma Priority (System.Default_Priority + 2);
--     end Step_Task;
--
--     task body Step_Task is
--        Next_Period : Time := Clock;
--     begin
--        Ada.Synchronous_Task_Control.Suspend_Until_True (Task_Sync);
--
--        Stepper.Set_Stepper_Callbacks
--          (Set_Step              => Set_Step_Pin'Access,
--           Clear_Step            => Clear_Step_Pin'Access,
--           Set_Direcetion        => Set_Step_Direction'Access,
--           Set_Stepper_Frequency => Set_Stepper_Frequency'Access,
--           Home_Test             => Home_Test'Access,
--           Motor_Enable          => Motor_Enable'Access);
--
--        loop
--           Next_Period := Next_Period + Task_Period;
--
--           Set (Analysis_Point);
--           if Stepper.Execute_Step_Event then
--              null;
--           end if;
--           Clear (Analysis_Point);
--           delay until Next_Period;
--        end loop;
--     end Step_Task;

end Step_Control;
