
with FE310_SVD.GPIO;   use FE310_SVD.GPIO;
with FE310_SVD.UART;   use FE310_SVD.UART;
with HAL;              use HAL;
with System;           use System;
with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;   use System.Machine_Code;

package body Console is

   CLINT_Addr            : constant := 16#02000000#;
   CLINT_Mtime_Offset    : constant := 16#BFF8#;

   Mtime_Lo_Addr : constant Address := To_Address (CLINT_Addr + CLINT_Mtime_Offset);
   Mtime_Hi_Addr : constant Address := To_Address (CLINT_Addr + CLINT_Mtime_Offset + 4);

   Mtime_Lo : UInt32 with Volatile_Full_Access, Address => Mtime_Lo_Addr;
   Mtime_Hi : UInt32 with Volatile_Full_Access, Address => Mtime_Hi_Addr;

   procedure Initialize;
   function CPU_Frequency return UInt32;

   generic
      Reg_Name : String;
      type Reg_Type is private;
   function Read_CSR return Reg_Type
     with Inline_Always;

   CPU_Frequency_Calc : UInt32 := 0;

   --------------
   -- Read_CSR --
   --------------

   function Read_CSR return Reg_Type is
      Ret : Reg_Type;
   begin
      Asm ("csrr %0, " & Reg_Name,
           Outputs  => Reg_Type'Asm_Output ("=r", Ret),
           Volatile => True);
      return Ret;
   end Read_CSR;

   function I_Mcycle_Low is new Read_CSR ("mcycle", UInt32);
   function Mcycle_Low return UInt32 renames I_Mcycle_Low;

   -------------------
   -- CPU_Frequency --
   -------------------

   function CPU_Frequency return UInt32 is

      function Measure_Freq (Count : UInt32) return UInt32;

      ------------------
      -- Measure_Freq --
      ------------------

      function Measure_Freq (Count : UInt32) return UInt32 is
         Start_Mtime     : UInt32;
         Delta_Mtime     : UInt32;
         Timer_Frequency : constant := 32768;
         Mtime_Freq      : constant UInt32 := Timer_Frequency;
         Start_Mcycle    : UInt32;
         Delta_Mcycle    : UInt32;
         Tmp             : UInt32;

      begin

         Tmp := Mtime_Lo;
         loop
            Start_Mtime := Mtime_Lo;
            exit when Start_Mtime /= Tmp;
         end loop;

         Start_Mcycle := Mcycle_Low;

         loop
            Delta_Mtime := Mtime_Lo - Start_Mtime;
            exit when Delta_Mtime > Count;
         end loop;

         Delta_Mcycle := Mcycle_Low - Start_Mcycle;

         return (Delta_Mcycle / Delta_Mtime) * Mtime_Freq
           + ((Delta_Mcycle mod Delta_Mtime) * Mtime_Freq) / Delta_Mtime;
      end Measure_Freq;

   begin
      if CPU_Frequency_Calc = 0 then
         --  Warm up
         CPU_Frequency_Calc := Measure_Freq (1);

         --  measure for real
         CPU_Frequency_Calc := Measure_Freq (10);
      end if;

      return CPU_Frequency_Calc;
   end CPU_Frequency;

   ----------
   -- Time --
   ----------

   function Time return HAL.UInt64 is
      Hi, Lo : UInt32;
   begin
      Hi := Mtime_Hi;
      Lo := Mtime_Lo;
      if Hi /= Mtime_Hi then
         return Shift_Left (UInt64 (Hi + 1), 32);
      else
         return Shift_Left (UInt64 (Hi), 32) + UInt64 (Lo);
      end if;
   end Time;

   ---------------------
   -- Timer_Frequency --
   ---------------------

   function Timer_Frequency return HAL.UInt64
   is (32768);

   -----------------------
   -- Wait_Milliseconds --
   -----------------------

   procedure Wait_Milliseconds (Ms : Natural) is
      Stop : constant HAL.UInt64 :=
        Time + (Timer_Frequency * HAL.UInt64 (Ms)) / 1000;
   begin
      while Time < Stop loop
         null;
      end loop;
   end Wait_Milliseconds;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      GPIO0_Periph.IO_FUNC_SEL.Arr (17) := False;
      GPIO0_Periph.IO_FUNC_SEL.Arr (18) := False;

      GPIO0_Periph.IO_FUNC_EN.Arr (18) := True;
      GPIO0_Periph.IO_FUNC_EN.Arr (17) := True;

      UART0_Periph.DIV.DIV := UInt16 ((CPU_Frequency / 115200)) - 1;
      UART0_Periph.TXCTRL.ENABLE := True;

      for I in 1 .. 1_000 loop
         null;
      end loop;
   end Initialize;

   -----------
   -- Print --
   -----------

   procedure Print (C : Character) is
   begin
      while UART0_Periph.TXDATA.FULL loop
         null;
      end loop;

      UART0_Periph.TXDATA.DATA := Character'Pos (C);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Str : String) is
   begin
      for C of Str loop
         Print (C);
      end loop;
   end Print;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line (Str : String) is
   begin
      Print (Str);
      Print (ASCII.CR);
      Print (ASCII.LF);
   end Print_Line;

begin
   Initialize;
end Console;
