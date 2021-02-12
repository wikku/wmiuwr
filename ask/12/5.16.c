void inner7(vec_ptr u, vec_ptr v, data_t *dest) {
	long i;
	long length = vec_length(u);
	data_t *udata = get_vec_start(u);
	data_t *vdata = get_vec_start(v);
	data_t sum = (data_t) 0;
	for (i = 0; i < length - 6; i += 6) {
		sum = sum + (udata[i]   * vdata[i]
		          +  udata[i+1] * vdata[i+1]
		          +  udata[i+2] * vdata[i+2]
		          +  udata[i+3] * vdata[i+3]
		          +  udata[i+4] * vdata[i+4]
		          +  udata[i+5] * vdata[i+5]);
	}
	for (; i < length; i++){
		sum = sum + udata[i] * vdata[i];
	}
	*dest = sum;
}

/* A. W każdej wersji iloczynu skalarnego będziemy musieli wykonać
 * mnożenie i dzielenie dla każdego elementu tablicy.
 * Throughput mnożenia liczb całkowitych to 1.0, tyle samo wynosi throughput
 * dodawania liczb zmiennoprzecinkowych, więc to jest dolne ograniczenie.
 *
 * B. dodawanie liczb zmiennoprzecinkowych ma latency 3.0, a
 * jest zależność sumy między każdymi kolejnymi iteracjami
 */
