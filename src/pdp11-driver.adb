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
with Pdp11.Images;
with Pdp11.Options;
with Pdp11.Paths;
with Pdp11.Tests;
with Pdp11.Tools;

procedure Pdp11.Driver is
   Machine  : Pdp11.Machine.Instance;

   function Ask (Prompt : String) return String;

   ---------
   -- Ask --
   ---------

   function Ask (Prompt : String) return String is
   begin
      Ada.Text_IO.Put (Prompt);
      Ada.Text_IO.Flush;
      return Ada.Text_IO.Get_Line;
   end Ask;

begin

   WL.Command_Line.Load_Defaults (".pdp11-options");

   if Pdp11.Options.Test_Encoding then
      Pdp11.Tests.Test_Encoding;
      Pdp11.Tests.Test_Decoding;
      return;
   end if;

   if Pdp11.Options.Create_Disk then
      declare
         Disk_File : constant String :=
                       (if Pdp11.Options.Disk_File = ""
                        then Ask ("disk file: ")
                        else Pdp11.Options.Disk_File);
      begin
         Pdp11.Tools.Create_Disk_Image ("RF11", Disk_File);
      end;
      return;
   end if;

   if Pdp11.Options.Write_Disk then
      declare
         Disk_File : constant String :=
                       (if Pdp11.Options.Disk_File = ""
                        then Ask ("disk file: ")
                        else Pdp11.Options.Disk_File);
         Object_File : constant String :=
                         (if Pdp11.Options.Object = ""
                          then Ask ("object file: ")
                          else Pdp11.Options.Object);
         Disk_Address : constant String :=
                          (if Pdp11.Options.Disk_Address = ""
                           then Ask ("disk address: ")
                           else Pdp11.Options.Disk_Address);
         Start_Offset : constant Natural :=
                          Natural'Value ("8#" & Disk_Address & "#");
      begin
         Pdp11.Tools.Disk_Write ("RF11", Disk_File, Object_File, Start_Offset);
      end;
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

      Start : constant Word_16 :=
                (if Pdp11.Options.Start /= 0
                 then Word_16 (Pdp11.Options.Start)
                 else Pdp11.Config.Get_Config ("start"));

      procedure Install_Device
        (Command : String);

      --------------------
      -- Install_Device --
      --------------------

      procedure Install_Device
        (Command : String)
      is
         Device : constant Pdp11.Devices.Reference :=
                    Pdp11.Devices.Loader.Load (Command, Memory);
      begin
         Machine.Add_Device (Device);
      end Install_Device;

   begin

      Machine.Create (Memory);

      Pdp11.Config.Iterate_Config
        ("device", Install_Device'Access);

      if Pdp11.Options.Object /= "" then
         Install_Device ("rom " & Pdp11.Images.Octal_Image (Start)
                         & " " & Pdp11.Options.Object);
      end if;

      Machine.Set_Register (7, Start);
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
