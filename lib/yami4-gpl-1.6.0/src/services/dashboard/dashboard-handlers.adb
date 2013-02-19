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

with Dashboard.Actions;
with Dashboard.Configuration;
with Dashboard.HTML;
with Log;

with IAL.Utils;

with AWS.Messages;
with AWS.MIME;
with AWS.Parameters;
with AWS.URL;

with Ada.Calendar.Formatting;
with Ada.Characters.Latin_1;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Exceptions;
with Ada.Streams;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;

package body Dashboard.Handlers is
   
   --
   --  Container for the handler index.
   --
   
   type Handler_Access is access function (Request : in AWS.Status.Data)
                                          return AWS.Response.Data;
   
   package Handlers_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => String,
      Element_Type => Handler_Access,
      Hash => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   
   Handlers : Handlers_Maps.Map;
   
   --
   --  Helpers.
   --
   
   LF : Character renames Ada.Characters.Latin_1.LF;
   procedure Append (Source   : in out Ada.Strings.Unbounded.Unbounded_String;
                     New_Item : in String)
     renames Ada.Strings.Unbounded.Append;
   
   function Response_From_File (Filename : in String)
                               return AWS.Response.Data is
   begin
      Log.Put (HTTP, "returning regular file " & Filename);
      
      return AWS.Response.File (AWS.MIME.Content_Type (Filename), Filename);
   end Response_From_File;
   
   --
   --  Main handler that dispatches HTTP requests to specific handler.
   --
   function Main_Handler (Request : in AWS.Status.Data)
                         return AWS.Response.Data is
      URI : constant String := AWS.Status.URI (Request);
      
      C : Handlers_Maps.Cursor;
      
      use type Handlers_Maps.Cursor;
      
   begin      
      --  search the index of specific handlers
      
      C := Handlers.Find (URI);
      if C /= Handlers_Maps.No_Element then
         
         --  dispatch to specific handler
         
         Log.Put (HTTP, "processing specific handler " & URI);
         
         return Handlers_Maps.Element (C).all (Request);
         
      else
         
         --  return the content of regular file
         
         if URI = "/" then
            return Response_From_File ("files/index.html");
         else
            return Response_From_File ("files" & URI);
         end if;
      end if;
   exception
      when E : others =>
         declare
            Msg : constant String := Ada.Exceptions.Exception_Message (E);
            
            Response : constant String := 
              HTML.Head &
              "<div class=""error"">" & LF &
              "<p>Cannot process this operation for the following reason:</p>" & LF &
              "<p class=""error_msg"">" & Msg & "</p>" & LF &
              "</div>" &
              HTML.Tail;
         begin
            Log.Put (HTTP, Msg);
         
            return AWS.Response.Build
              (Content_Type => AWS.MIME.Text_HTML,
               Message_Body => Response,
               Status_Code => AWS.Messages.S500);
         end;
   end Main_Handler;
   
   --
   --  Individual handlers.
   --
   
   function Name_Servers (Request : in AWS.Status.Data)
                         return AWS.Response.Data is
      
      Location : constant String :=
        AWS.Status.Parameter (Request, "location");
      
      Response : Ada.Strings.Unbounded.Unbounded_String;      
      Index : Positive;
      
      function List_Of_Name_Servers return AWS.Response.Data is
      begin
         Append
           (Response,
            HTML.Head & LF &
              "<h2>YAMI4 Name Servers</h2>" & LF &
              "<p>List of known name servers:</p>" & LF &
              "<ul>" & LF);
      
         Index := 1;
         loop
            declare
               Name_Server_Location : constant String :=
                 Configuration.Names_Location (Index);
            begin
               exit when Name_Server_Location = "";
               Append (Response,
                       "   <li><a href=""/ns?location=" &
                         AWS.URL.Encode (Name_Server_Location) & """>" &
                         Name_Server_Location & "</a></li>" & LF);
            end;
            Index := Index + 1;
         end loop;
           
         Append (Response, "</ul>" & LF);
         Append (Response,
                 "<p>Select any name server to display its registered names.</a></p>" &
                   HTML.Tail);
            
         return AWS.Response.Build
           (Content_Type => AWS.MIME.Text_HTML,
            UString_Message => Response);
      end List_Of_Name_Servers;
      
      function Manage_Single_Name_Server (Location : in String)
                                         return AWS.Response.Data is
         
         Entries : Actions.Name_Server_Entries :=
           Actions.Get_Name_Entries (Location);
         
         C : Actions.Name_Server_Entry_Vectors.Cursor;
      begin
         Append
           (Response,
            HTML.Head & LF &
              "<h2>YAMI4 Name Servers</h2>" & LF &
              "<h3>" & Location & "</h3>" & LF &
              "<p>List of registered names:</p>" & LF &
              "<table class=""list"">" & LF &
              "  <tr><th>Object</th><th>Location</th></tr>" & LF);
         
         C := Entries.First;
         while Actions.Name_Server_Entry_Vectors.Has_Element (C) loop
            declare
               E : Actions.Name_Server_Entry
                 renames Actions.Name_Server_Entry_Vectors.Element (C);
               
               Loc : constant String := Ada.Strings.Unbounded.To_String (E.Location);
            begin
               Append
                 (Response,
                  "  <tr>" & LF &
                    "    <td>" & Ada.Strings.Unbounded.To_String (E.Name) & "</td>" & LF &
                    "    <td><a href=""/ping?location=" & AWS.URL.Encode (Loc) &
                    """>" & Loc & "</a></td>" & LF &
                    "  </tr>" & LF);
            end;
            Actions.Name_Server_Entry_Vectors.Next (C);
         end loop;
         
         Append (Response,
                 "</table>" & LF &
                   "<p>Click on the location to ping it.</p>" & LF);
         Append (Response,
                 "<p>Add new name server entry:</p>" & LF &
                   "<form action=""/ns-add?location=" & Location & """ method=""post"">" & LF &
                   "<table>" & LF &
                   "  <tr>" & LF &
                   "    <td>Object:</td>" & LF &
                   "    <td><input type=""text"" size=""40"" name=""object"" /></td>" & LF &
                   "  </tr>" & LF &
                   "  <tr>" & LF &
                   "    <td>Location:</td>" & LF &
                   "    <td><input type=""text"" size=""40"" name=""objlocation"" /></td>" & LF &
                   "  </tr>" & LF &
                   "  <tr>" & LF &
                   "    <td>&nbsp;</td>" & LF &
                   "    <td><input type=""submit"" value=""  Add  "" /></td>" & LF &
                   "  </tr>" & LF &
                   "</table>" & LF &
                   "</form>" & LF &
                   "<p><a href=""/ns"">Go back to the list of name servers.</a></p>" &
                   HTML.Tail);
         
         return AWS.Response.Build
           (Content_Type => AWS.MIME.Text_HTML,
            UString_Message => Response);
      end Manage_Single_Name_Server;
      
   begin
      if Location = "" then
         return List_Of_Name_Servers;
      else
         return Manage_Single_Name_Server (Location);
      end if;
   end Name_Servers;
   
   function Name_Servers_Add_Entry (Request : in AWS.Status.Data)
                                   return AWS.Response.Data is
      
      NS_Location : constant String :=
        AWS.Status.Parameter (Request, "location");
      
      Object : constant String :=
        AWS.Status.Parameter (Request, "object");
      
      Object_Location : constant String :=
        AWS.Status.Parameter (Request, "objlocation");
      
      Response : Ada.Strings.Unbounded.Unbounded_String;      
      
   begin
      Actions.Add_Name_Server_Entry (NS_Location, Object, Object_Location);
      
      Append
        (Response,
         HTML.Head & LF &
           "<h2>YAMI4 Name Servers</h2>" & LF &
           "<h3>" & NS_Location & "</h3>" & LF &
           "<p>New name server entry:</p>" &
           "<table class=""list"">" & LF &
           "  <tr><th>Object</th><th>Location</th></tr>" & LF &
           "  <tr>" & LF &
           "    <td>" & Object & "</td>" & LF &
           "    <td>" & Object_Location & "</td>" & LF &
           "  </tr>" & LF &
           "</table>" & LF &
           "<p>was added successfully.</p>" & LF &
           "<p><a href=""/ns?location=" & NS_Location & """>Continue with this server</a>" &
           " or <a href=""/ns"">go back to the list of name servers.</a></p>" & LF &
           HTML.Tail);
         
      return AWS.Response.Build
        (Content_Type => AWS.MIME.Text_HTML,
         UString_Message => Response);
   end Name_Servers_Add_Entry;
   
   function Brokers (Request : in AWS.Status.Data)
                    return AWS.Response.Data is
      
      Location : constant String :=
        AWS.Status.Parameter (Request, "location");
      
      Response : Ada.Strings.Unbounded.Unbounded_String;      
      Index : Positive;
      
      function List_Of_Brokers return AWS.Response.Data is
      begin
         Append
           (Response,
            HTML.Head & LF &
              "<h2>YAMI4 Brokers</h2>" & LF &
              "<p>List of known message brokers:</p>" & LF &
              "<ul>" & LF);
      
         Index := 1;
         loop
            declare
               Broker_Location : constant String :=
                 Configuration.Broker_Location (Index);
            begin
               exit when Broker_Location = "";
               Append (Response,
                       "   <li><a href=""/brokers?location=" &
                         AWS.URL.Encode (Broker_Location) & """>" &
                         Broker_Location & "</a></li>" & LF);
            end;
            Index := Index + 1;
         end loop;
           
         Append (Response, "</ul>" & LF);
         Append (Response,
                 "<p>Select any broker to display its statistics.</a></p>" &
                   HTML.Tail);
            
         return AWS.Response.Build
           (Content_Type => AWS.MIME.Text_HTML,
            UString_Message => Response);
      end List_Of_Brokers;
      
      function Show_Broker_Stats return AWS.Response.Data is
         
         Stats : constant Actions.Broker_Stats :=
           Actions.Get_Broker_Stats (Location);
         
         C : Actions.Subscriber_Stats_Vectors.Cursor;
         
      begin
         Append
           (Response,
            HTML.Head & LF &
              "<h2>YAMI4 Brokers</h2>" & LF &
              "<h3>" & Location & "</h3>" & LF &
              "<table>" & LF &
              "  <tr><td>Total incoming messages:</td><td>" &
              Ada.Streams.Stream_Element_Count'Image (Stats.Total_Incoming) &
              "</td></tr>" & LF &
              "  <tr><td>Total outgoing messages:</td><td>" &
              Ada.Streams.Stream_Element_Count'Image (Stats.Total_Outgoing) &
              "</td></tr>" & LF &
              "</table>" & LF);
         
         if Stats.Sub_Stats.Is_Empty then
            Append
              (Response,
               "<p>There are no active subscriptions now.</p>" & LF);
         else
            Append
              (Response,
               "<p>Statistics for individual subscriptions:</p>" & LF &
                 "<table class=""list"">" & LF &
                 "  <tr><th>Location</th><th>Overflow</th><th>Messages</th><th>Bytes</th></tr>"
                 & LF);
            
            C := Stats.Sub_Stats.First;
            while Actions.Subscriber_Stats_Vectors.Has_Element (C) loop
               declare
                  S : Actions.Subscriber_Stats
                    renames Actions.Subscriber_Stats_Vectors.Element (C);
               begin
                  Append
                    (Response,
                     "  <tr>" & LF &
                       "    <td>" & Ada.Strings.Unbounded.To_String (S.Location) & "</td>" & LF);
                  if S.Overflow then
                     Append (Response, "    <td>(!)</td>" & LF);
                  else
                     Append (Response, "    <td>&nbsp;</td>" & LF);
                  end if;
                  
                  Append
                    (Response,
                     "    <td class=""number"">" & Natural'Image (S.Message_Count) & "</td>" & LF &
                       "    <td class=""number"">" &
                       Ada.Streams.Stream_Element_Count'Image (S.Byte_Count) & "</td>" & LF &
                       "  </tr>" & LF);
                  
                  Actions.Subscriber_Stats_Vectors.Next (C);
               end;
            end loop;
            
            Append (Response, "</table>" & LF);
         end if;
         
         Append (Response, HTML.Tail);
         
         return AWS.Response.Build
           (Content_Type => AWS.MIME.Text_HTML,
            UString_Message => Response);
      end Show_Broker_Stats;
      
   begin
      if Location = "" then
         return List_Of_Brokers;
      else
         return Show_Broker_Stats;
      end if;
   end Brokers;
   
   function Queues (Request : in AWS.Status.Data)
                    return AWS.Response.Data is
      
      Location : constant String :=
        AWS.Status.Parameter (Request, "location");
      
      Response : Ada.Strings.Unbounded.Unbounded_String;      
      Index : Positive;
      
      function List_Of_Queues return AWS.Response.Data is
      begin
         Append
           (Response,
            HTML.Head & LF &
              "<h2>YAMI4 Queue Servers</h2>" & LF &
              "<p>List of known queue servers:</p>" & LF &
              "<ul>" & LF);
      
         Index := 1;
         loop
            declare
               Queue_Location : constant String :=
                 Configuration.Queues_Location (Index);
            begin
               exit when Queue_Location = "";
               Append (Response,
                       "   <li><a href=""/queues?location=" &
                         AWS.URL.Encode (Queue_Location) & """>" &
                         Queue_Location & "</a></li>" & LF);
            end;
            Index := Index + 1;
         end loop;
           
         Append (Response, "</ul>" & LF);
         Append (Response,
                 "<p>Select any queue server to display its statistics.</a></p>" &
                   HTML.Tail);
            
         return AWS.Response.Build
           (Content_Type => AWS.MIME.Text_HTML,
            UString_Message => Response);
      end List_Of_Queues;
      
      function Show_Queue_Stats return AWS.Response.Data is
         
         Stats : constant Actions.All_Queue_Stats :=
           Actions.Get_All_Queue_Stats (Location);
         
         C : Actions.Queue_Stats_Vectors.Cursor;
         
      begin
         Append
           (Response,
            HTML.Head & LF &
              "<h2>YAMI4 Queue Servers</h2>" & LF &
              "<h3>" & Location & "</h3>" & LF);

         if Stats.Num_Of_Queues /= 0 then
            Append
              (Response,
               "<table>" & LF &
               "  <tr><td>Number of active queues:</td><td>" &
               Natural'Image (Stats.Num_Of_Queues) &
               "</td></tr>" & LF &
               "  <tr><td>Number of all waiting messages:</td><td>" &
               Natural'Image (Stats.Num_Of_All_Waiting_Messages) &
               "</td></tr>" & LF &
               "  <tr><td>Total size of all waiting messages:</td><td>" &
               Ada.Streams.Stream_Element_Count'Image (Stats.Total_Of_All_Content_Sizes) &
               "</td></tr>" & LF &
               "  <tr><td>Number of all waiting clients:</td><td>" &
               Natural'Image (Stats.Num_Of_All_Waiting_Clients) &
               "</td></tr>" & LF &
               "</table>" & LF);

            Append
              (Response,
               "<p>Statistics for individual queues:</p>" & LF &
                 "<table class=""list"">" & LF &
                 "  <tr><th>Name</th><th>Num of messages</th>" &
                 "<th>Size of messages</th><th>Num of clients</th></tr>" & LF);

            C := Stats.Queue_Stats.First;
            while Actions.Queue_Stats_Vectors.Has_Element (C) loop
               declare
                  S : Actions.Queue_Stats
                    renames Actions.Queue_Stats_Vectors.Element (C);
               begin
                  Append
                    (Response,
                     "  <tr>" & LF &
                       "    <td>" & Ada.Strings.Unbounded.To_String (S.Name) & "</td>" & LF);

                  Append
                    (Response,
                     "    <td class=""number"">" &
                     Natural'Image (S.Num_Of_Messages) & "</td>" & LF &
                     "    <td class=""number"">" &
                     Ada.Streams.Stream_Element_Count'Image (S.Size) & "</td>" & LF &
                     "    <td class=""number"">" &
                     Natural'Image (S.Num_Of_Clients) & "</td>" & LF &
                     "  </tr>" & LF);
                  
                  Actions.Queue_Stats_Vectors.Next (C);
               end;
            end loop;
            
            Append (Response, "</table>" & LF);

         else
            Append
              (Response,
               "<p>Currently there are no active queues on this server.</p>" & LF);
         end if;
         
         Append (Response, HTML.Tail);
         
         return AWS.Response.Build
           (Content_Type => AWS.MIME.Text_HTML,
            UString_Message => Response);
      end Show_Queue_Stats;
      
   begin
      if Location = "" then
         return List_Of_Queues;
      else
         return Show_Queue_Stats;
      end if;
   end Queues;
   
   function Ping (Request : in AWS.Status.Data)
                 return AWS.Response.Data is
      
      Location : constant String :=
        AWS.Status.Parameter (Request, "location");
      
      Response : Ada.Strings.Unbounded.Unbounded_String;
      
      procedure Show_Ping is
         
         Stats : constant Actions.Ping_Stats := Actions.Ping (Location);
         
         procedure Process_String_In_Vector
           (Position : in Actions.String_Vectors.Cursor) is
         begin
            Append
              (Response,
               Actions.String_Vectors.Element (Position) & "<br />");
         end Process_String_In_Vector;
         
         procedure Process_Connection_In_Vector
           (Position : in Actions.Connection_Stats_Vectors.Cursor) is
            
            Conn_Stats : Actions.Connection_Stats renames
              Actions.Connection_Stats_Vectors.Element (Position);
            
         begin
            Append
              (Response,
               "  <tr>" & LF &
                 "    <td>" & Ada.Strings.Unbounded.To_String (Conn_Stats.Remote) & "</td>" & LF &
                 "    <td class=""number"">" &
                 Integer'Image (Conn_Stats.Messages_Received) & "</td>" & LF &
                 "    <td class=""number"">" &
                 Integer'Image (Conn_Stats.Messages_Sent) & "</td>" & LF &
                 "    <td class=""number"">" &
                 Ada.Streams.Stream_Element_Count'Image (Conn_Stats.Bytes_Received) & "</td>" & LF &
                 "    <td class=""number"">" &
                 Ada.Streams.Stream_Element_Count'Image (Conn_Stats.Bytes_Sent) & "</td>" & LF &
                 "  </tr>" & LF);
         end Process_Connection_In_Vector;
         
      begin
         Append (Response, "<h3>" & Location & "</h3>" & LF);
         
         if Stats.Connection_Successful then
            Append
              (Response,
               "<p>Connection open in " &
                 Ada.Calendar.Formatting.Image
                 (Elapsed_Time => Stats.Connection_Time,
                 Include_Time_Fraction => True) &
                 "</p>" & LF);
            
            if Stats.Message_Successful then
               Append
                 (Response,
                  "<p>Statistics obtained in " &
                    Ada.Calendar.Formatting.Image
                    (Elapsed_Time => Stats.Connection_Time,
                     Include_Time_Fraction => True) &
                    "</p>" & LF);
               
               Append
                 (Response,
                  "<table class=""list"">" & LF &
                    "  <tr>" & LF &
                    "    <td>Agent uptime</td>" & LF &
                    "    <td>" & IAL.Utils.Long_Duration_Image (Stats.Uptime) & "</td>" & LF &
                    "  </tr>" & LF &
                    "  <tr>" & LF &
                    "    <td>Listeners</td>" & LF &
                    "    <td>");
               
               Stats.Listeners.Iterate (Process_String_In_Vector'Access);
               
               Append
                 (Response,
                  "    </td>" & LF &
                    "  </tr>" & LF &
                    "  <tr>" & LF &
                    "    <td>Objects</td>" & LF &
                    "    <td>");
               
               Stats.Objects.Iterate (Process_String_In_Vector'Access);
               
               Append
                 (Response,
                  "    </td>" & LF &
                    "  </tr>" & LF &
                    "  <tr>" & LF &
                    "    <td>Connections</td>" & LF &
                    "    <td>" & LF &
                    "<table class=""list"">" & LF &
                    "  <tr>" & LF &
                    "    <th>Target</th>" &
                    "<th>Messages received</th><th>Messages sent</th>" &
                    "<th>Bytes received</th><th>Bytes sent</th>" & LF &
                    "  </tr>" & LF);
               
               Stats.Connections.Iterate (Process_Connection_In_Vector'Access);
               
               Append
                 (Response,
                  "</table>" & LF &
                    "    </td>" & LF &
                    "  </tr>" & LF &
                    "</table>" & LF &
                    "<p>&nbsp;</p>" & LF);
               
            else
               Append
                 (Response,
                  "<p class=""error_msg"">The stats message was not processed properly.</p>");
            end if;
         else
            Append (Response,
                    "<p class=""error_msg"">Cannot connect to this target.</p>");
         end if;
      end Show_Ping;
      
   begin
      Append (Response, HTML.Head & LF & "<h2>YAMI4 Ping</h2>" & LF);
      
      if Location /= "" then
         Show_Ping;
      end if;
      
      Append
        (Response,
         "<form action=""/ping"" method=""post"">" & LF &
           "<table>" & LF &
           "  <tr>" & LF &
           "    <td>Location:</td>" & LF &
           "    <td><input type=""text"" size=""40"" name=""location"" value=""" &
           Location & """ /></td>" & LF &
           "    <td><input type=""submit"" value=""  Ping  "" /></td>" & LF &
           "  </tr>" & LF &
           "</table>" & LF &
           "</form>" & LF);
      
      return AWS.Response.Build
        (Content_Type => AWS.MIME.Text_HTML,
         UString_Message => Response);
   end Ping;
   
begin
   
   --  Register all specific handlers.
   
   Handlers.Insert ("/ns",      Name_Servers'Access);
   Handlers.Insert ("/ns-add",      Name_Servers_Add_Entry'Access);
   
   Handlers.Insert ("/brokers", Brokers'Access);
   
   Handlers.Insert ("/queues",  Queues'Access);

   Handlers.Insert ("/ping",    Ping'Access);
   
end Dashboard.Handlers;
