#Hello!
import aiohttp
import json
import asyncio
import async_timeout
import logging
import sys
import time
import datetime
import socket
import ssl

API_KEY = "whatever_your_key_is"

servers = ['Goloman', 'Hands', 'Holiday', 'Welsh', 'Wilkes']
serv_to_port = {'Goloman': 11490, 'Hands': 11491, 'Holiday': 11492, 'Welsh': 11493, 'Wilkes': 11494}
connect_map = {'Goloman': ['Hands', 'Holiday', 'Wilkes'], 'Hands': ['Goloman', 'Wilkes'],
               'Holiday': ['Goloman', 'Welsh', 'Wilkes'], 'Wilkes': ['Goloman', 'Hands', 'Holiday'], 'Welsh': ['Holiday']}

tasks = {}

client_devices = {}

# a prototype function to test the asyncio's capability in server creation
async def handle_echo(reader, writer):
    data = await reader.read(100)
    message = data.decode()
    addr = writer.get_extra_info('peername')
    print("Received %r from %r" % (message, addr))
    
    print("Send: %r" % message)
    writer.write(data)
    writer.drain()

    print("Close the client socket")
    writer.close()
#-----------WRITING TO THE CLIENT----------#

async def write_to_client(writer, msg):
        if msg == None:
                return
        try:
            writer.write(msg.encode())
            # print("writing response:\n" + msg)
            await writer.drain()
            writer.write_eof()
        except:
            pass
        return


#-----------WRITING TO THE LOGFILE----------#

async def write_to_log(msg):
        if msg == None:
                return
        try:
            log_file.write(msg)
        except:
            pass

    
#-----------FLOODING ALGORITHM----------#

async def flooding(c):
    client_name = client_devices[c]
    at_cmd = 'AT %s %s %s %s %s %s' % (c, client_name['server_name'], client_name['latitude'], client_name['longitude'],str(client_name['time_difference']), str(client_name['command_time']))
    
    for node in connect_map[server_name]:
        port = serv_to_port[node]
        try:
            # print('Propagating %s to %s' % (c, node))
            reader, writer = await asyncio.open_connection('127.0.0.1', port, loop=serv_loop)
            await write_to_log('CONNECTED TO ' + node + '\n')
            await write_to_log('PROPOGATING %s TO %s:%s\n' % (c, node, at_cmd))
            await write_to_client(writer,at_cmd)
            await write_to_log('CLOSED CONNECTION WITH ' + node + '\n\n')
        except:
            # print('Error propogating to ' + node)
            await write_to_log('ERROR PROPAGATING %s to %s\n\n' % (c,node))


#-----------AT COMMAND PROCESSING (ONLY SERVER TO SERVER)----------#
async def at(writer, client, server_name, lat, lon, time_diff, ctime):
    if client in client_devices and float(client_devices[client]['command_time']) >= float(ctime):
        await write_to_log('REDUNDANT COMMAND SENT FOR: ' + client + '\n\n')
        return
    
    client_devices[client] = {
    'server_name': server_name,
    'latitude' : lat,
    'longitude' : lon,
    'time_difference' : float(time_diff),
    'command_time' : float(ctime)
    }
    await flooding(client)
                       

#-----------FETCHING INFORMATION FROM POPULATED URL----------#

async def fetch(session, url):
    async with async_timeout.timeout(10):
        async with session.get(url) as response:
            return await response.json()


#-----------WHATSAT COMMAND PROCESSING----------#

async def whatsat(writer, command, ctime, client, rad, num_res):
    if client not in client_devices:
        await write_to_log('CLIENT NOT IN DEVICES LIST: ' + client + '\n')
        tmp_msg = '? ' + command
        await write_to_log('SENDING: ' + tmp_msg + '\n')
        await write_to_client(writer,tmp_msg)
        return None

    get_lat = client_devices[client]["latitude"]
    get_lon = client_devices[client]["longitude"]

    url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=%s,%s&radius=%d&key=%s'  % (get_lat, get_lon, float(rad), API_KEY)

    # Since in this example, the server.py file is acting like a client to google's API!
    # that's why the async with keyword instead of just async!
    async with aiohttp.ClientSession() as session:
        await write_to_log('QUERYING GOOGLE PLACES API FOR LOCATION(S): (%s,%s) with radius: %s\n'
                       % (get_lat,get_lon,rad))

        response = await fetch(session, url)
        res = response['results'][:int(num_res)]
        t_diff = client_devices[client]["time_difference"]
        latlon = get_lat + get_lon
        serv_resp = 'AT %s %s %s %s %s %s' %  (server_name, str(t_diff), client, latlon, ctime,
                                            json.dumps(res, indent=3))

        await write_to_log('RESPONDING TO WHATSAT MESSAGE:\n' + serv_resp + '\n\n')

        await write_to_client(writer,serv_resp)
        # print('Writing response: ' + serv_resp + '\n')


#-----------IAMAT COMMAND PROCESSING----------#

async def iamat(writer, rtime, client, latlon, ctime):
    time_delay = rtime - float(ctime)

    # Processing to get the lat and lon vals from the passed string
    ind = 0
    count = 0
    lat = ''
    lon = ''
    for c in latlon:
        if c == '+' or c == '-':
            count = count + 1
            if count == 2:
                lat = latlon[:(ind-1)]
                lon = latlon[ind:]
        ind = ind + 1

    # Since we can assume that the client will only give us up to date commands with time stamps
    # in correct chronological order with command times.
    # So we can just focus on updating the client devices list
    client_devices[client] = {
        'server_name': server_name,
        'latitude' : lat,
        'longitude' : lon,
        'time_difference' : float(time_delay),
        'command_time' : float(ctime)
    }
    #print(client_devices[client])
    
    serv_resp = 'AT %s %s %s %s %s' %  (server_name, str(time_delay), client, latlon, ctime)
    await write_to_log('GIVING RESPONSE TO IAMAT COMMAND: \n' + serv_resp + '\n')
    await write_to_client(writer,serv_resp)
    await flooding(client)
    # print('Writing response: ' + serv_resp)
    

#-----------OVERALL MANAGING COMMUNICATION AND INTERPRETING TYPE----------#

async def manage_commands(writer, full_cmd, split_cmd):
    # separating passed args into the parts that we want
    comd_type = split_cmd[0]
    time_received = time.time()
    list_len = len(split_cmd)
    comd_list = ['IAMAT' , 'WHATSAT' , 'AT']
    # Dealing with logging invalid commands!
    if full_cmd != '' and comd_type not in comd_list:
        await write_to_log('RESPONDING TO INVALID COMMAND: \n' + '? ' + full_cmd + '\n')
        # print("Invalid command sent to server...")
        send_full = '? ' + full_cmd
        await write_to_client(writer,send_full)
        # print("Response writing: " + full_cmd)
        return None
        
    elif comd_type == comd_list[1] and list_len < 4:
        await write_to_log('RESPONDING TO INVALID COMMAND: \n' + '? ' + full_cmd \
+ '\n')
        send_full = '? ' + full_cmd
        await write_to_client(writer,send_full)
        return None

    # Logging the passed command to logfile
    if(len(split_cmd) > 3):
        await write_to_log("COMMAND RECEIVED: \n " + full_cmd +  ' @ ' + str(time_received) + '\n\n')
    # print("COMMAND RECEIVED: \n " + full_cmd + '@' + str(time_received) + '\n')
    
    #Now getting into dealing with the three valid command cases
    if comd_type == comd_list[0]: #This would be IAMAT
        await iamat(writer, time_received, split_cmd[1], split_cmd[2], split_cmd[3])        
    elif comd_type == comd_list[1]: #This would be WHATSAT
        await whatsat(writer,full_cmd,time_received,split_cmd[1], split_cmd[2], split_cmd[3]) 
    elif comd_type == comd_list[2]: #This would be AT, for server-server comm
        await at(writer, split_cmd[1],split_cmd[2],split_cmd[3],split_cmd[4],split_cmd[5],
                 split_cmd[5])

async def handle_client_reqs(reader,writer):
    while not reader.at_eof():
        command = await reader.readline()
        split_cmd = command.decode().split(' ')
        await manage_commands(writer,command.decode(),split_cmd)        


async def server_client_conn(reader, writer):
    # wrapping the coroutine in the task, schedule execution
    task = asyncio.create_task(handle_client_reqs(reader,writer))
    # as list of reader-writer pairs, allowing for equal and separate clients and sever
    # interactions
    tasks[task] = (reader,writer)
    def close_client_resp(task):
        log_file.write("Closing  Connection...\n\n")
        #print("Closing client...")
        del tasks[task]
        writer.close()

    task.add_done_callback(close_client_resp)


#-----------MAIN----------#
if __name__ == '__main__':
    # Argument check 
    if(len(sys.argv) != 2):
        print('Wrong number of arguments: needs appropriate server name and port')
        exit(1)
    
    # Getting server name from args
    global server_name
    server_name = sys.argv[1]
    if not server_name in servers:
        print('Server passed is not a part of the valid severs list: {}'.format(servers))
        exit(1)
    
    # Getting the correlated port number for the server from above dict
    port_num = serv_to_port[server_name]

    # Setting up a logging system with basic fine handler
    log = server_name + "-log.txt"
    global log_file
    full_name = "./logs/" + log
    open(full_name, "w").close() #apparently clears the file!
    log_file = open(full_name, 'a+')
    log_file.write(server_name + '\n')

    # defining the server through the event loop and run_until_complete(which uses the start_server),
    # all from asyncio
    global serv_loop 
    serv_loop = asyncio.get_event_loop()
    coro = asyncio.start_server(server_client_conn, '127.0.0.1', port_num, loop=serv_loop)
    server = serv_loop.run_until_complete(coro)

    print('Serving on {}'.format(server.sockets[0].getsockname()))
    log_file.write('Serving on {}'.format(server.sockets[0].getsockname()) + '\n')

    try:
        serv_loop.run_forever()
    except KeyboardInterrupt:
        pass

        # Close the server
    server.close()
    serv_loop.run_until_complete(server.wait_closed())
    serv_loop.close()
