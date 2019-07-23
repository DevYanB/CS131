import aiohttp
import json
import asyncio
import logging
import sys
import time
import datetime
import socket
import ssl

curr = 11490

async def tcp_echo_client(loop,t):
    test_msg = "IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 " + str(t) + '\n'
    reader,writer = await asyncio.open_connection('127.0.0.1' , curr, loop=loop)
    try:
        print('Sending: ' + test_msg)
        writer.write(test_msg.encode())
        await writer.drain()
        while not reader.at_eof():
            info = await reader.read(10000)
            print('%s' % info.decode())
            break

    except KeyboardInterrupt:
        print('Closing connection')
        writer.close()
        return

loop = asyncio.get_event_loop()
loop.run_until_complete(tcp_echo_client(loop,time.time()))
loop.close
