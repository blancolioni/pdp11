package Pdp11.Paths is

   Config_Path : constant String :=
     "D:\git\pdp11\config";

   function Config_File
     (File_Path : String)
     return String
   is (Config_Path & "/" & File_Path);

end Pdp11.Paths;
