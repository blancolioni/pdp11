with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

with WL.Command_Line;

with Pdp11.Devices;
with Pdp11.ISA;
with Pdp11.Machine;

with Pdp11.Addressable.Memory;

with Pdp11.Config;
with Pdp11.Devices.Loader;
with Pdp11.Options;
with Pdp11.Paths;
with Pdp11.Tests;

procedure Pdp11.Driver is
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
      return;
   end if;

   if not Ada.Directories.Exists (".pdp11-config") then
      Ada.Directories.Copy_File
        (Source_Name => Pdp11.Paths.Config_File ("init.pdp11"),
         Target_Name => ".pdp11-config");
   end if;

   Pdp11.Config.Load_Configuration (".pdp11-config");

   declare
      Memory : constant Pdp11.Addressable.Memory.Memory_Reference :=
                 Pdp11.Addressable.Memory.Create;

      procedure Install_Device
        (Command : String);

      --------------------
      -- Install_Device --
      --------------------

      procedure Install_Device
        (Command : String)
      is
         Device : constant Pdp11.Devices.Reference :=
                    Pdp11.Devices.Loader.Load (Command);
      begin
         Machine.Add_Device (Device);
      end Install_Device;

      --  Device_List : constant array (Positive range <>)
      --    of Pdp11.Devices.Reference
      --      := (Pdp11.Devices.Line_Clock.Create,
      --          Pdp11.Devices.RAM.Create (0, 255),
      --          Pdp11.Devices.RAM.Create (16#1000#, 16#1FFF#),
      --          Pdp11.Devices.ROM.Create (Base_Address, Object),
      --          Pdp11.Devices.RF11.Create,
      --          Pdp11.Devices.TTY.Create);

   begin

      Machine.Create (Memory);

      Pdp11.Config.Iterate_Config
        ("device", Install_Device'Access);

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

      if Pdp11.Options.Start /= 0 then
         Machine.Set_Register (7, Word_16 (Pdp11.Options.Start));
      else
         Machine.Set_Register
           (7, Pdp11.Config.Get_Config ("start"));
      end if;

   end;

   if Pdp11.Options.Execute then
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

   if not Pdp11.Options.Quiet then
      Machine.Report;
   end if;

end Pdp11.Driver;
