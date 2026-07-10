import 'package:firebase_core/firebase_core.dart' show FirebaseOptions;
import 'package:flutter/foundation.dart' show defaultTargetPlatform, kIsWeb, TargetPlatform;


class DefaultFirebaseOptions {
  static FirebaseOptions get currentPlatform {
    if (kIsWeb) {
      return FirebaseOptions(
        apiKey: String.fromEnvironment('FIREBASE_WEB_API_KEY'),
        appId: String.fromEnvironment('FIREBASE_WEB_APPID'),
        messagingSenderId: String.fromEnvironment('FIREBASE_MESSAGINGSENDERID'),
        projectId: String.fromEnvironment('FIREBASE_PROJECTID'),
        authDomain: String.fromEnvironment('FIREBASE_AUTHDOMAIN'),
        storageBucket: String.fromEnvironment('FIREBASE_STORAGEBUCKET'),
        measurementId: String.fromEnvironment('FIREBASE_WEB_MEASUREMENTID'),
      );
    }
    switch (defaultTargetPlatform) {
      case TargetPlatform.android:
        return FirebaseOptions(
          apiKey: String.fromEnvironment('FIREBASE_ANDROID_API_KEY'),
          appId: String.fromEnvironment('FIREBASE_ANDROID_APPID'),
          messagingSenderId: String.fromEnvironment('FIREBASE_MESSAGINGSENDERID'),
          projectId: String.fromEnvironment('FIREBASE_PROJECTID'),
          storageBucket: String.fromEnvironment('FIREBASE_STORAGEBUCKET'),
        );
      case TargetPlatform.iOS:
        return  FirebaseOptions(
            apiKey: String.fromEnvironment('FIREBASE_IOS_MACOS_API_KEY'),
            appId: String.fromEnvironment('FIREBASE_IOS_MACOS_APPID'),
            messagingSenderId: String.fromEnvironment('FIREBASE_MESSAGINGSENDERID'),
            projectId: String.fromEnvironment('FIREBASE_PROJECTID'),
            storageBucket: String.fromEnvironment('FIREBASE_STORAGEBUCKET'),
            iosBundleId: String.fromEnvironment('FIREBASE_IOS_MACOS_IOSBUNDLEID')
        );
      case TargetPlatform.macOS:
        return FirebaseOptions(
            apiKey: String.fromEnvironment('FIREBASE_IOS_MACOS_API_KEY'),
            appId: String.fromEnvironment('FIREBASE_IOS_MACOS_APPID'),
            messagingSenderId: String.fromEnvironment('FIREBASE_MESSAGINGSENDERID'),
            projectId: String.fromEnvironment('FIREBASE_PROJECTID'),
            storageBucket: String.fromEnvironment('FIREBASE_STORAGEBUCKET'),
            iosBundleId: String.fromEnvironment('FIREBASE_IOS_MACOS_IOSBUNDLEID')
        );
      case TargetPlatform.windows:
        return FirebaseOptions(
          apiKey: String.fromEnvironment('FIREBASE_WINDOWS_API_KEY'),
          appId: String.fromEnvironment('FIREBASE_WINDOWS_APPID'),
          messagingSenderId: String.fromEnvironment('FIREBASE_MESSAGINGSENDERID'),
          projectId: String.fromEnvironment('FIREBASE_PROJECTID'),
          authDomain: String.fromEnvironment('FIREBASE_AUTHDOMAIN'),
          storageBucket: String.fromEnvironment('FIREBASE_STORAGEBUCKET'),
          measurementId: String.fromEnvironment('FIREBASE_WINDOWS_MEASUREMENTID'),
        );
      case TargetPlatform.linux:
        throw UnsupportedError(
          'DefaultFirebaseOptions have not been configured for linux - '
              'you can reconfigure this by running the FlutterFire CLI again.',
        );
      default:
        throw UnsupportedError(
          'DefaultFirebaseOptions are not supported for this platform.',
        );
    }
  }
}
