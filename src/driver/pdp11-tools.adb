with Ada.Sequential_IO;
with System.Storage_Elements;

package body Pdp11.Tools is

   package Storage_IO is
     new Ada.Sequential_IO (System.Storage_Elements.Storage_Element);

   Disk_Unit_Count  : constant := 8;
   Track_Count      : constant := 128;
   Track_Length     : constant := 2048;
   Disk_Words       : constant := Track_Length * Track_Count;
   Disk_Bytes       : constant := Disk_Words * 2;
   Total_Disk_Store : constant := Disk_Unit_Count * Disk_Bytes;

   Disk_Image : System.Storage_Elements.Storage_Array (1 .. Total_Disk_Store);

   procedure Save_Disk (Path : String);
   procedure Load_Disk (Path : String);

   -----------------------
   -- Create_Disk_Image --
   -----------------------

   procedure Create_Disk_Image
     (Disk_Type       : String;
      Disk_Image_Path : String)
   is
      pragma Unreferenced (Disk_Type);
   begin
      Disk_Image := (others => 0);
      Save_Disk (Disk_Image_Path);
   end Create_Disk_Image;

   ----------------
   -- Disk_Write --
   ----------------

   procedure Disk_Write
     (Disk_Type       : String;
      Disk_Image_Path : String;
      From_File_Path  : String;
      Start_Offset    : Natural)
   is
      pragma Unreferenced (Disk_Type);
      use System.Storage_Elements;
      use Storage_IO;
      Source : File_Type;
      Offset : Storage_Offset :=
                 Storage_Offset (Start_Offset * 2 + 1);
   begin
      Load_Disk (Disk_Image_Path);
      Open (Source, In_File, From_File_Path);

      declare
         X : Storage_Element;
      begin
         while not End_Of_File (Source) loop
            Read (Source, X);
            Disk_Image (Offset) := X;
            Offset := Offset + 1;
         end loop;
      end;
      Close (Source);

      Save_Disk (Disk_Image_Path);

   end Disk_Write;

   ---------------
   -- Load_Disk --
   ---------------

   procedure Load_Disk (Path : String) is
      use Storage_IO;
      File : File_Type;
   begin
      Open (File, In_File, Path);
      for X of Disk_Image loop
         Read (File, X);
      end loop;
      Close (File);
   end Load_Disk;

   ---------------
   -- Save_Disk --
   ---------------

   procedure Save_Disk (Path : String) is
      use Storage_IO;
      File : File_Type;
   begin
      Create (File, Out_File, Path);
      for X of Disk_Image loop
         Write (File, X);
      end loop;
      Close (File);
   end Save_Disk;

end Pdp11.Tools;
