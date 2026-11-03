const
  ClientCrtPEM =
    '-----BEGIN CERTIFICATE-----' + LineEnding +
    'MIIBnDCCAUGgAwIBAgIUYhT91kBsSVE3rJKE1yK0UxUfJXkwCgYIKoZIzj0EAwIw' + LineEnding +
    'IjEgMB4GA1UEAwwXUGFzY2FsU0NBREEgdGVzdCBjbGllbnQwIBcNMjYxMTAyMjM1' + LineEnding +
    'ODI1WhgPMjEyNjEwMDkyMzU4MjVaMCIxIDAeBgNVBAMMF1Bhc2NhbFNDQURBIHRl' + LineEnding +
    'c3QgY2xpZW50MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEuiEAjbrWOiF2QZKo' + LineEnding +
    'CHFBAqiAtw2e2iVPFcpCIx2sNnHtbFgM63lRafy42tWQB30sY/xoeTlnzTZaKPfN' + LineEnding +
    '+S1PZKNTMFEwHQYDVR0OBBYEFN1NuWarV1d9Mjwc61cpwwmBxW+xMB8GA1UdIwQY' + LineEnding +
    'MBaAFN1NuWarV1d9Mjwc61cpwwmBxW+xMA8GA1UdEwEB/wQFMAMBAf8wCgYIKoZI' + LineEnding +
    'zj0EAwIDSQAwRgIhAIpxRh9FPtSf3kxJnvREk5598WtJjb2tFilJkNTZae66AiEA' + LineEnding +
    '859n16wqtyrks1NbN9N3tSJNuaTfaw+oxLD2wtFRNrw=' + LineEnding +
    '-----END CERTIFICATE-----';

  ClientKeyPEM =
    '-----BEGIN PRIVATE KEY-----' + LineEnding +
    'MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQgLd1q0qhH74rclqth' + LineEnding +
    'QXTRQQsN+ztE9tlFn+TSPgX8WOihRANCAAS6IQCNutY6IXZBkqgIcUECqIC3DZ7a' + LineEnding +
    'JU8VykIjHaw2ce1sWAzreVFp/Lja1ZAHfSxj/Gh5OWfNNloo9835LU9k' + LineEnding +
    '-----END PRIVATE KEY-----';

  ClientEncKeyPEM =
    '-----BEGIN ENCRYPTED PRIVATE KEY-----' + LineEnding +
    'MIHsMFcGCSqGSIb3DQEFDTBKMCkGCSqGSIb3DQEFDDAcBAhjGogFJ1tTDwICCAAw' + LineEnding +
    'DAYIKoZIhvcNAgkFADAdBglghkgBZQMEASoEEEYBcCPH4WaZ/K5p6yfct1MEgZBM' + LineEnding +
    'hb6Uu0oPhRpzahlbgkDtxMr081Ji94P78QzqLFL82EHbdrmqvkscH/FSJoV5v2UX' + LineEnding +
    'EWCm1tO2XeUb9YsVncuyInUfW5iGQ2jTDdDhSXqAU/7T8O+TSD/ASNnt+Ay2xECK' + LineEnding +
    'mKr/fMXWIRsjpiElTEExZH7DQUvFwgxwhvx44Ej/aG4qBuRFKGj+gFmtPc0dYaU=' + LineEnding +
    '-----END ENCRYPTED PRIVATE KEY-----';

  OtherKeyPEM =
    '-----BEGIN PRIVATE KEY-----' + LineEnding +
    'MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQgjEM0FyVvz5FtEYPN' + LineEnding +
    'Qkvw1/A5nWIdxIooQu3SyqNTrEyhRANCAATl/oWT62MiCDeugQXXh3yN9QVsZC9n' + LineEnding +
    'qFSdOP9MWtY6JB+safPqOty29+e13iavF596xaf/kjIrpzNB0WtSxhCW' + LineEnding +
    '-----END PRIVATE KEY-----';

  ServerCrtPEM =
    '-----BEGIN CERTIFICATE-----' + LineEnding +
    'MIIBmzCCAUGgAwIBAgIUCDn7RHFt0D6fkeMUv4m2fZSpB60wCgYIKoZIzj0EAwIw' + LineEnding +
    'IjEgMB4GA1UEAwwXUGFzY2FsU0NBREEgdGVzdCBzZXJ2ZXIwIBcNMjYxMTAyMjM1' + LineEnding +
    'ODI1WhgPMjEyNjEwMDkyMzU4MjVaMCIxIDAeBgNVBAMMF1Bhc2NhbFNDQURBIHRl' + LineEnding +
    'c3Qgc2VydmVyMFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEU6NjaRbM0gll9rmd' + LineEnding +
    'VcrnaSLMQO42Aj1xdJJCZ1LmLC6MqjbteV8WBTe2qnohFl7pTYXMINlDrRuDt2VQ' + LineEnding +
    'aSCFz6NTMFEwHQYDVR0OBBYEFNZ8UE8b5GHfoV1rUCCCfXyve9aWMB8GA1UdIwQY' + LineEnding +
    'MBaAFNZ8UE8b5GHfoV1rUCCCfXyve9aWMA8GA1UdEwEB/wQFMAMBAf8wCgYIKoZI' + LineEnding +
    'zj0EAwIDSAAwRQIgVThx+iD83CP0Y8isTESvDAfI+r0n81uadqS41ghSmFQCIQDh' + LineEnding +
    'CwubT+j4SQP8LyDaGkDhKbKS8E7Wslix75Zz/1uvdg==' + LineEnding +
    '-----END CERTIFICATE-----';

  ServerKeyPEM =
    '-----BEGIN PRIVATE KEY-----' + LineEnding +
    'MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQgASAh47TIY5u3LTxN' + LineEnding +
    '9D1ahKpK7htlnMG/e5F3uNTqCy6hRANCAARTo2NpFszSCWX2uZ1VyudpIsxA7jYC' + LineEnding +
    'PXF0kkJnUuYsLoyqNu15XxYFN7aqeiEWXulNhcwg2UOtG4O3ZVBpIIXP' + LineEnding +
    '-----END PRIVATE KEY-----';

  ClientP12_B64 =
      'MIIEZQIBAzCCBBsGCSqGSIb3DQEHAaCCBAwEggQIMIIEBDCCAqIGCSqGSIb3'
    + 'DQEHBqCCApMwggKPAgEAMIICiAYJKoZIhvcNAQcBMFcGCSqGSIb3DQEFDTBK'
    + 'MCkGCSqGSIb3DQEFDDAcBAi4AIVMcOD5lAICCAAwDAYIKoZIhvcNAgkFADAd'
    + 'BglghkgBZQMEASoEEJh68K8VynaFAnTnVd7TIZ6AggIgZkUtbf2TGDoWUiBP'
    + 'EFdJ+88Fdkb42rz8SSfOnuBYBIeVKgkFnix2MGwnn6l4EAJSsQi8tOYz+3xN'
    + 'Y1+HwbP+O0Wc9nnE/yk6CrR6vCwB3edoRNaJm92V+LoJ4bG3xwIgIOsaS3D1'
    + 'QrTG1F+mPJXai/ioRRGfDkjzk0WjTc+fXA1rUU3HO7/n5fGBuFxaZ6UWslBb'
    + 'Rncolk7/t+LNAZIGcKApt2sr+Tv/xI2e3VL5Tto02PG1VQJcKDRpINFFMt7j'
    + '3hVLTL5psdHERsoUE9QrybqejpL9WDdM18AO4su2qFquybO+yVf9L4fbnHl2'
    + 'AaBFEtRke979a6CmmVwyHLu2agLNMXsv8yvFdxu66n0w6GWGPxdewgGYZZwZ'
    + 'px/r4n3yl9FniMsxnBePdMXh81TZju+1XH807aq0R0hXb3ZFxXPjpMg6LnIy'
    + 'yhJ4aYhsaCKBlrg6/Nl09rA7IpQdqUyWrCPOMofshk0eiaM1Geldx5rY3iyP'
    + 'hAvOThqzIvqvGPygytTOn/ufmjgfk5BeMK9JpWLj5iWL51KA8TBPq+DB2INC'
    + 'qlFdcuyLZOWwHuRqFf5czHy+hQ6JSv+LqpK7k6gsRNlxl4Qy31yVrbwOCzRI'
    + 'hxa5mFeoOacGRnDLrC8sH7mtBtrMhGnHAeNpNIFWnwbDxN/i1dGkLExuMm2u'
    + 'F4ia+b9Z9v6TAR4h3w7393M58QSjwqSA9larYEAwlrNq1e761jCCAVoGCSqG'
    + 'SIb3DQEHAaCCAUsEggFHMIIBQzCCAT8GCyqGSIb3DQEMCgECoIHvMIHsMFcG'
    + 'CSqGSIb3DQEFDTBKMCkGCSqGSIb3DQEFDDAcBAiJr92oC4URwQICCAAwDAYI'
    + 'KoZIhvcNAgkFADAdBglghkgBZQMEASoEELaKcdmMaO/4saIMSqN3J4oEgZDS'
    + 'd/4R3WlVPAP9sKpbjRomS563PBvgO2U751AKjtA6zR0Xjx8sLI7biI2yvFs6'
    + 'Jit0Go/5vCXj8eWvgo2fxNS8gfAomQNNUq7+nS4ZlteeQXdk1fmNWovTfI4Y'
    + 'eYqxsc3j6sDo0a3U6YsKwfo2ql/7VxUA7NFoD5NF5IWE05c6qjSY7BLEKaZn'
    + 'C4tv9nC+eJMxPjAXBgkqhkiG9w0BCRQxCh4IAHQAZQBzAHQwIwYJKoZIhvcN'
    + 'AQkVMRYEFFbz7l77dzmoUJfVWV0k4ciJqEQuMEEwMTANBglghkgBZQMEAgEF'
    + 'AAQg84SpahH9AcDBBg7ZPRli49USEGha9qe3GYJ0Y0/rzUkECP+X76aFt7PU'
    + 'AgIIAA==';

