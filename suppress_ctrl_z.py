def delete_eof(fin, fout):
	BUFSIZE = 2**15
	EOFCHAR = chr(26)
	data = fin.read(BUFSIZE)
	while data:
		fout.write(data.translate(None, EOFCHAR))
		data = fin.read(BUFSIZE)

import sys
ipath = sys.argv[1]
opath = ipath + ".new"
with open(ipath, "rb") as fin, open(opath, "wb") as fout:
	delete_eof(fin, fout)

