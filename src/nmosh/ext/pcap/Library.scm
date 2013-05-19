(mosh-pcap
  c-function-table
  *internal*
  (plugin: mosh_pcap)
  (libname: mosh_pcap)
  (c-import: "mpcap.c")
  #(ret name args)
  (void* mpcap_findalldevs)
  )
