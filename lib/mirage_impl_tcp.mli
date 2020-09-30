type 'a tcp

val tcp : 'a tcp Functoria.typ

type tcpv4 = Mirage_impl_ip.v4 tcp

type tcpv6 = Mirage_impl_ip.v6 tcp

type tcpv4v6 = Mirage_impl_ip.v4v6 tcp

val tcpv4 : tcpv4 Functoria.typ

val tcpv6 : tcpv6 Functoria.typ

val tcpv4v6 : tcpv4v6 Functoria.typ

val direct_tcp :
     ?clock:Mirage_impl_mclock.mclock Functoria.impl
  -> ?random:Mirage_impl_random.random Functoria.impl
  -> ?time:Mirage_impl_time.time Functoria.impl
  -> 'a Mirage_impl_ip.ip Functoria.impl
  -> 'a tcp Functoria.impl

val socket_tcpv4 : ?group:string -> Ipaddr.V4.t option -> tcpv4 Functoria.impl

val keyed_socket_tcpv4 : Ipaddr.V4.Prefix.t Mirage_key.key -> tcpv4 Functoria.impl

val socket_tcpv6 : ?group:string -> Ipaddr.V6.t option -> tcpv6 Functoria.impl

val keyed_socket_tcpv6 : Ipaddr.V6.Prefix.t option Mirage_key.key -> tcpv6 Functoria.impl

val socket_tcpv4v6 : ?group:string -> Ipaddr.V4.t option -> Ipaddr.V6.t option -> tcpv4v6 Functoria.impl

val keyed_socket_tcpv4v6 : Ipaddr.V4.Prefix.t Mirage_key.key -> Ipaddr.V6.Prefix.t option Mirage_key.key -> tcpv4v6 Functoria.impl
