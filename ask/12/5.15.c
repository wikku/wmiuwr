void inner6(vec_ptr u, vec_ptr v, data_t *dest) {
	long i;
	long length = vec_length(u);
	data_t *udata = get_vec_start(u);
	data_t *vdata = get_vec_start(v);
	data_t sum0 = sum1 = sum2 = sum3 = sum4 = sum5 = (data_t) 0;
	for (i = 0; i < length - 6; i += 6) {
		sum0 = sum0 + data[i]   * vdata[i];
		sum1 = sum1 + data[i+1] * vdata[i+1];
		sum2 = sum2 + data[i+2] * vdata[i+2];
		sum3 = sum3 + data[i+3] * vdata[i+3];
		sum4 = sum4 + data[i+4] * vdata[i+4];
		sum5 = sum5 + data[i+5] * vdata[i+5];
	}
	for (; i < length; i++){
		sum0 = sum0 + udata[i] * vdata[i];
	}
	*dest = sum0 + sum1 + sum2 + sum3 + sum4 + sum5;
}

/* A. W każdej wersji iloczynu skalarnego będziemy musieli wykonać
 * mnożenie i dzielenie dla każdego elementu tablicy.
 * Throughput mnożenia liczb całkowitych to 1.0, tyle samo wynosi throughput
 * dodawania liczb zmiennoprzecinkowych, więc to jest dolne ograniczenie.
 *
 */
