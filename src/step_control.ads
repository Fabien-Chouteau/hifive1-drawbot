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

with Gcode; use Gcode;

with HAL.GPIO; use HAL.GPIO;
with HiFive1; use HiFive1;

package Step_Control is
   procedure Initalize;
private

   subtype Drawbot_Axes is Axis_Name range X_Axis .. Y_Axis;

   type GPIO_Point_Per_Axis is array (Drawbot_Axes) of not null Any_GPIO_Point;

   Step_GPIO : GPIO_Point_Per_Axis :=
     (X_Axis => HF1_Pin_12'Access,
      Y_Axis => HF1_Pin_7'Access);

   Dir_GPIO : GPIO_Point_Per_Axis :=
     (X_Axis => HF1_Pin_5'Access,
      Y_Axis => HF1_Pin_8'Access);

   Not_Enable_GPIO : GPIO_Point_Per_Axis :=
     (X_Axis => HF1_Pin_4'Access,
      Y_Axis => HF1_Pin_4'Access);
end Step_Control;
