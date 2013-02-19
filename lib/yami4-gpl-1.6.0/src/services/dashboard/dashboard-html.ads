--  Copyright Maciej Sobczak 2008-2012.
--  This file is part of YAMI4.
--
--  YAMI4 is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  YAMI4 is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with YAMI4.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Characters.Latin_1;
use Ada.Characters.Latin_1;

package Dashboard.HTML is
   
   Head : constant String :=
     "<!DOCTYPE html PUBLIC ""-//W3C//DTD XHTML 1.0 Transitional//EN"" ""http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"">" & LF &
     "<html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en'>" & LF &
     "<head>" & LF &
     "  <meta http-equiv=""Content-Type"" content=""text/html; charset=ISO-8859-1"" />" & LF &
     "  <title>YAMI4 Dashboard</title>" & LF &
     "  <link rel=""stylesheet"" type=""text/css"" href=""style.css"" />" & LF &
     "</head>" & LF &
     "<body>" & LF &
     "<table class=""home_banner"" border=""0"" cellpadding=""0"" cellspacing=""0"">" & LF &
     "  <tr>" & LF &
     "    <td><a href=""/""><img src=""banner.png"" alt=""Inspirel banner"" /></a></td>" & LF &
     "  </tr>" & LF &
     "</table>" & LF & LF;
   
   Tail : constant String := LF &
     "</body>" & LF &
     "</html>" & LF;

end Dashboard.HTML;
