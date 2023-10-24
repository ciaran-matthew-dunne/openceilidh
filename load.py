import subprocess
import json
import base64
import xml.etree.ElementTree as ET
import zipfile
import io

def extract_musical_score(file_path):
    cmd = f"musescore4portable --score-media {file_path}"
    result = subprocess.check_output(cmd, shell=True)
    return json.loads(result)

def decode_musicxml(encoded_string):
    decoded_bytes = base64.b64decode(encoded_string)
    with zipfile.ZipFile(io.BytesIO(decoded_bytes), 'r') as z:
        with z.open('score.xml') as f:
            return f.read().decode('utf-8')
# Use the above functions
file_path = "/home/ciaran/prog/openceilidh/tunes/mscz/Willafjord.mscz"
data = extract_musical_score(file_path)
xml_string = decode_musicxml(data['mxml'])

print(xml_string)

# Parse XML (assuming you want to print out all notes)
# root = ET.fromstring(xml_string)
# for note in root.findall(".//note"):
#     print(note)
