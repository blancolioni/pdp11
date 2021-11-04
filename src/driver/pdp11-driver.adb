with Ada.Command_Line;                 use Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

with Pdp11.Assembler;
with Pdp11.ISA;
with Pdp11.Machine;

with Pdp11.Drivers.RAM;
with Pdp11.Drivers.ROM;
with Pdp11.Drivers.TTY;

with Pdp11.Paths;
with Pdp11.Tests;

procedure Pdp11.Driver is
   Assembly : Pdp11.Assembler.Assembly_Type;
   Machine  : Pdp11.Machine.Machine_Type;
   Source   : constant String :=
                (if Argument_Count = 1
                 then Argument (1)
                 else Pdp11.Paths.Config_File ("test_tty1.m11"));
   Output   : constant String :=
                Pdp11.Paths.Config_File
                  (Ada.Directories.Base_Name (Source) & ".o");
begin

   Pdp11.Tests.Test_Encoding;
   Pdp11.Tests.Test_Decoding;

   Assembly.Load (Source);
   Assembly.Link;
   Assembly.Save (Output);

   Machine.Add_Driver
     (Driver => Pdp11.Drivers.ROM.Create_ROM_Driver (Output),
      Base   => 16#C000#);
   Machine.Add_Driver
     (Driver => Pdp11.Drivers.RAM.Create_RAM (4095),
      Base   => 4096);
   Machine.Add_Driver
     (Driver => Pdp11.Drivers.TTY.TTY_Driver,
      Base   => 16#FF80#);
   Machine.Set_Register (7, 16#C000#);
   Machine.Set_Register (6, 16#1FFE#);

   declare
      Quantum : constant Pdp11.ISA.Microsecond_Duration := 10_000.0;
      Used    : Pdp11.ISA.Microsecond_Duration;
   begin
      Machine.Execute (Quantum, Used);
   exception
      when Pdp11.Machine.Halted =>
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Halted");
      when E : others =>
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
   end;

   Machine.Report;

end Pdp11.Driver;
