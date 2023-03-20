with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

with WL.Command_Line;

with Pdp11.Assembler;
with Pdp11.ISA;
with Pdp11.Machine;

with Pdp11.Addressable.Memory;

with Pdp11.Devices.Line_Clock;
with Pdp11.Devices.RAM;
with Pdp11.Devices.ROM;
with Pdp11.Devices.TTY;

with Pdp11.Options;
with Pdp11.Paths;
with Pdp11.Tests;

procedure Pdp11.Driver is
   Assembly : Pdp11.Assembler.Assembly_Type;
   Machine  : Pdp11.Machine.Instance;

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
         declare
            Memory : constant Pdp11.Addressable.Memory.Memory_Reference :=
                       Pdp11.Addressable.Memory.Create;
            Device_List : constant array (Positive range <>)
              of Pdp11.Devices.Reference
                := (Pdp11.Devices.Line_Clock.Create,
                    Pdp11.Devices.RAM.Create (0, 255),
                    Pdp11.Devices.RAM.Create (4096, 4095),
                    Pdp11.Devices.ROM.Create (Base_Address, Output),
                    Pdp11.Devices.TTY.Create);

         begin

            Machine.Create (Memory);

            for Device of Device_List loop
               Machine.Add_Device (Device);
            end loop;

            --  Memory.Add_Device
            --   (Driver => Pdp11.Drivers.ROM.Create_ROM_Driver (Output),
            --    Base   => Base_Address);
            --  Memory.Add_Driver
            --   (Driver => Pdp11.Drivers.RAM.Create_RAM (4095),
            --    Base   => 4096);
            --  Memory.Add_Driver
            --   (Driver => Pdp11.Drivers.RAM.Create_RAM (256),
            --    Base   => 0);
            --  Memory.Add_Driver
            --   (Driver => Pdp11.Drivers.TTY.TTY_Driver,
            --    Base   => 16#FF80#);

            Machine.Set_Register (7, Word_16 (Base_Address));
            Machine.Set_Register (6, 16#1FFE#);
         end;

         begin
            if Pdp11.Options.Time_Limit = 0 then
               Machine.Start;
            else
               Machine.Execute_Quantum
                 (Pdp11.ISA.Microsecond_Duration
                    (Pdp11.Options.Time_Limit));
            end if;
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
