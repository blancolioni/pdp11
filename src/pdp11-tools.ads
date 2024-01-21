package Pdp11.Tools is

   procedure Create_Disk_Image
     (Disk_Type       : String;
      Disk_Image_Path : String);

   procedure Disk_Write
     (Disk_Type       : String;
      Disk_Image_Path : String;
      From_File_Path  : String;
      Start_Offset    : Natural);

end Pdp11.Tools;
