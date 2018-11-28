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

with FE310.GPIO; use FE310.GPIO;
with HiFive1; use HiFive1;
with FE310.PWM; use FE310.PWM;
with FE310.Device; use FE310.Device;

package Step_Control is
   procedure Initalize;

   type Pen_Position is (High, Low);
   type Eraser_Position is (High, Low);

   procedure Set_Pen (Pos : Pen_Position);
   procedure Set_Eraser (Pos : Eraser_Position);

private

   PWM       : PWM_Device renames PWM1;

   Pen_Cmp_ID    : constant Comparator_ID := 1;
   Pen_Low_Duty  : constant := 65;
   Pen_High_Duty : constant := 140;
   Pen_IO        : GPIO_Point renames HF1_Pin_3;

   Eraser_Cmp_ID    : constant Comparator_ID := 3;
   Eraser_Low_Duty  : constant := 90;
   Eraser_High_Duty : constant := 35;
   Eraser_IO        : GPIO_Point renames HF1_Pin_6;

   PWM_Scale  : constant := 8;
   PWM_Period : constant := 1300;
   --  Duty:  30 -> 0.5ms
   --  Duty: 100 -> 1.5ms
   --  Duty: 170 -> 2.5ms

   Enable_Motors_IO : GPIO_Point renames HF1_Pin_4;
   Step_Motor_X     : GPIO_Point renames HF1_Pin_12;
   Dir_Motor_X      : GPIO_Point renames HF1_Pin_5;
   Step_Motor_Y     : GPIO_Point renames HF1_Pin_7;
   Dir_Motor_Y      : GPIO_Point renames HF1_Pin_8;

end Step_Control;
