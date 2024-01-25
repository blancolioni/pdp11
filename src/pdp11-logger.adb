with Ada.Text_IO;

package body Pdp11.Logger is

   Log_File : Ada.Text_IO.File_Type;
   Logging  : Boolean := False;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      if Logging then
         Ada.Text_IO.New_Line (Log_File);
      end if;
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (Text : String) is
   begin
      if Logging then
         Ada.Text_IO.Put (Log_File, Text);
      end if;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Text : String) is
   begin
      Put (Text);
      New_Line;
   end Put_Line;

   -----------
   -- Start --
   -----------

   procedure Start (Path : String) is
   begin
      Ada.Text_IO.Create (Log_File, Ada.Text_IO.Out_File, Path);
      Logging := True;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      if Logging then
         Ada.Text_IO.Close (Log_File);
      end if;
   end Stop;


end Pdp11.Logger;
