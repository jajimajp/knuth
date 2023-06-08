theories/Loader.vo theories/Loader.glob theories/Loader.v.beautified theories/Loader.required_vo: theories/Loader.v src/META.coq-plugin-tutorial src/tuto0_plugin$(DYNOBJ)
theories/Loader.vio: theories/Loader.v src/META.coq-plugin-tutorial src/tuto0_plugin$(DYNOBJ)
theories/Loader.vos theories/Loader.vok theories/Loader.required_vos: theories/Loader.v src/META.coq-plugin-tutorial src/tuto0_plugin$(DYNOBJ)
theories/Demo.vo theories/Demo.glob theories/Demo.v.beautified theories/Demo.required_vo: theories/Demo.v theories/Loader.vo
theories/Demo.vio: theories/Demo.v theories/Loader.vio
theories/Demo.vos theories/Demo.vok theories/Demo.required_vos: theories/Demo.v theories/Loader.vos
