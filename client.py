#!/usr/bin/env python
import pika
import os
import re
import subprocess

clientnum = "7"
hostname = "client" + clientnum
myregex = re.compile(r"RELOAD %s (\d+)" % clientnum)
neutered = False
credentials = pika.PlainCredentials('reloader', 'eFS6RaYegNB8')
parameters = pika.ConnectionParameters(credentials=credentials)

connection = pika.BlockingConnection(pika.ConnectionParameters(host='127.0.0.1',virtual_host='/extensions',credentials=credentials))
channel = connection.channel()
channel.exchange_declare(exchange='rExchange',
                         exchange_type='fanout',durable=True)
channel.queue_declare(queue=hostname,durable=True)
channel.queue_bind(queue=hostname,exchange='rExchange')

def callback_neutered(ch, method, properties, body):
    print(" [x] Received %r" % body)

def callback_real(ch, method, properties, body):
    print(" [x] Received %r" % body)
    delay = myregex.search(body)
    if delay:
        thedelay = delay.group(1)
        print(" [x] Found %r" % thedelay)
#        subprocess.call("/bin/bash -c \"sleep %r && asterisk -rx 'dialplan reload'\"" % thedelay,shell=True,stderr=subprocess.STDOUT)
callback = callback_neutered if neutered else callback_real        

channel.basic_consume(callback,
                      queue=hostname,
                      no_ack=True)

print(' [*] Waiting for messages. To exit press CTRL+C')
channel.start_consuming()
