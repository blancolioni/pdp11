with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

with WL.Command_Line;

with Pdp11.Assembler;
with Pdp11.ISA;
with Pdp11.Machine;

with Pdp11.Drivers.RAM;
with Pdp11.Drivers.ROM;
with Pdp11.Drivers.TTY;

with Pdp11.Options;
with Pdp11.Paths;
with Pdp11.Tests;

procedure Pdp11.Driver is
   Assembly : Pdp11.Assembler.Assembly_Type;
   Machine  : Pdp11.Machine.Machine_Type;
begin

   if not Ada.Directories.Exists (".pdp11-options") then
      Ada.Directories.Copy_File
        (Source_Name => Pdp11.Paths.Config_File ("default-options.txt"),
         Target_Name => ".pdp11-options");
   end if;

   WL.Command_Line.Load_Defaults (".pdp11-options");

   if Pdp11.Options.Test_Encoding then
      Pdp11.Tests.Test_Encoding;
      Pdp11.Tests.Test_Decoding;
   end if;

   declare
      Source : constant String :=
                 Pdp11.Options.Source;
      Output   : constant String :=
                   (if Source = ""
                    then Pdp11.Options.Object
                    else Ada.Directories.Base_Name (Source) & ".o");
      Base_Address : constant Address_Type :=
                       Address_Type (Pdp11.Options.Base_Address);
   begin
      if Source /= "" then
         Assembly.Load (Source);
         Assembly.Link;
         Assembly.Save (Output);
      end if;

      if Output /= "" and then Pdp11.Options.Execute then
         Machine.Add_Driver
           (Driver => Pdp11.Drivers.ROM.Create_ROM_Driver (Output),
            Base   => Base_Address);
         Machine.Add_Driver
           (Driver => Pdp11.Drivers.RAM.Create_RAM (4095),
            Base   => 4096);
         Machine.Add_Driver
           (Driver => Pdp11.Drivers.RAM.Create_RAM (256),
            Base   => 0);
         Machine.Add_Driver
           (Driver => Pdp11.Drivers.TTY.TTY_Driver,
            Base   => 16#FF80#);
         Machine.Set_Register (7, Word_16 (Base_Address));
         Machine.Set_Register (6, 16#1FFE#);

         declare
            Time_Limit : constant Natural := Pdp11.Options.Time_Limit;
            Quantum    : constant Pdp11.ISA.Microsecond_Duration :=
                           (if Time_Limit = 0
                            then Pdp11.ISA.Microsecond_Duration'Last
                            else Pdp11.ISA.Microsecond_Duration (Time_Limit));
            Used       : Pdp11.ISA.Microsecond_Duration;
         begin
            Machine.Execute (Quantum, Used);
         exception
            when Pdp11.Machine.Halted =>
               if not Pdp11.Options.Quiet then
                  Ada.Text_IO.New_Line;
                  Ada.Text_IO.Put_Line ("Halted");
               end if;
            when E : others =>
               Ada.Text_IO.New_Line;
               Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         end;
      end if;
   end;

   if not Pdp11.Options.Quiet then
      Machine.Report;
   end if;

end Pdp11.Driver;
