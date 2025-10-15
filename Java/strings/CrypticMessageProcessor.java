import java.util.*;
import java.security.SecureRandom;

public class CrypticMessageProcessor {
    private static final String ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 ";
    private static final String REVERSE_ALPHABET = new StringBuilder(ALPHABET).reverse().toString();
    private static SecureRandom random = new SecureRandom();
    
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        
        System.out.println("🔐 Cryptic Message Processor 2015");
        System.out.println("1. Encode Message");
        System.out.println("2. Decode Message");
        System.out.println("3. Analyze Text");
        System.out.print("Choose operation (1-3): ");
        
        int choice = scanner.nextInt();
        scanner.nextLine(); // consume newline
        
        switch(choice) {
            case 1:
                System.out.print("Enter message to encode: ");
                String encodeMsg = scanner.nextLine();
                String encoded = encodeMessage(encodeMsg);
                System.out.println("Encoded: " + encoded);
                break;
            case 2:
                System.out.print("Enter message to decode: ");
                String decodeMsg = scanner.nextLine();
                String decoded = decodeMessage(decodeMsg);
                System.out.println("Decoded: " + decoded);
                break;
            case 3:
                System.out.print("Enter text to analyze: ");
                String analyzeMsg = scanner.nextLine();
                analyzeText(analyzeMsg);
                break;
            default:
                System.out.println("Invalid choice!");
        }
        
        scanner.close();
    }
    
    private static String encodeMessage(String message) {
        // Apply multiple transformations
        String step1 = reverseSubstitution(message);
        String step2 = vowelShift(step1);
        String step3 = insertRandomChars(step2);
        String step4 = railFenceCipher(step3);
        
        System.out.println("Encoding steps:");
        System.out.println("1. Reverse substitution: " + step1);
        System.out.println("2. Vowel shift: " + step2);
        System.out.println("3. Random chars added: " + step3);
        System.out.println("4. Rail fence applied: " + step4);
        
        return step4;
    }
    
    private static String decodeMessage(String message) {
        // Reverse the encoding steps
        String step1 = railFenceDecipher(message);
        String step2 = removeRandomChars(step1);
        String step3 = reverseVowelShift(step2);
        String step4 = reverseSubstitution(step3);
        
        System.out.println("Decoding steps:");
        System.out.println("1. Rail fence decoded: " + step1);
        System.out.println("2. Random chars removed: " + step2);
        System.out.println("3. Vowel shift reversed: " + step3);
        System.out.println("4. Reverse substitution: " + step4);
        
        return step4;
    }
    
    private static String reverseSubstitution(String text) {
        StringBuilder result = new StringBuilder();
        for (char c : text.toCharArray()) {
            int index = ALPHABET.indexOf(c);
            if (index != -1) {
                result.append(REVERSE_ALPHABET.charAt(index));
            } else {
                result.append(c);
            }
        }
        return result.toString();
    }
    
    private static String vowelShift(String text) {
        char[] chars = text.toCharArray();
        for (int i = 0; i < chars.length; i++) {
            char c = chars[i];
            switch(c) {
                case 'a': chars[i] = 'e'; break;
                case 'e': chars[i] = 'i'; break;
                case 'i': chars[i] = 'o'; break;
                case 'o': chars[i] = 'u'; break;
                case 'u': chars[i] = 'a'; break;
                case 'A': chars[i] = 'E'; break;
                case 'E': chars[i] = 'I'; break;
                case 'I': chars[i] = 'O'; break;
                case 'O': chars[i] = 'U'; break;
                case 'U': chars[i] = 'A'; break;
            }
        }
        return new String(chars);
    }
    
    private static String reverseVowelShift(String text) {
        char[] chars = text.toCharArray();
        for (int i = 0; i < chars.length; i++) {
            char c = chars[i];
            switch(c) {
                case 'a': chars[i] = 'u'; break;
                case 'e': chars[i] = 'a'; break;
                case 'i': chars[i] = 'e'; break;
                case 'o': chars[i] = 'i'; break;
                case 'u': chars[i] = 'o'; break;
                case 'A': chars[i] = 'U'; break;
                case 'E': chars[i] = 'A'; break;
                case 'I': chars[i] = 'E'; break;
                case 'O': chars[i] = 'I'; break;
                case 'U': chars[i] = 'O'; break;
            }
        }
        return new String(chars);
    }
    
    private static String insertRandomChars(String text) {
        StringBuilder result = new StringBuilder();
        for (char c : text.toCharArray()) {
            result.append(c);
            if (random.nextBoolean() && Character.isLetterOrDigit(c)) {
                result.append((char) (random.nextInt(26) + 'a'));
            }
        }
        return result.toString();
    }
    
    private static String removeRandomChars(String text) {
        StringBuilder result = new StringBuilder();
        for (int i = 0; i < text.length(); i++) {
            result.append(text.charAt(i));
            if (i + 1 < text.length() && Character.isLetter(text.charAt(i + 1)) && 
                Character.isLetter(text.charAt(i))) {
                i++; // skip the next random character
            }
        }
        return result.toString();
    }
    
    private static String railFenceCipher(String text) {
        StringBuilder rail1 = new StringBuilder();
        StringBuilder rail2 = new StringBuilder();
        
        for (int i = 0; i < text.length(); i++) {
            if (i % 2 == 0) {
                rail1.append(text.charAt(i));
            } else {
                rail2.append(text.charAt(i));
            }
        }
        return rail1.toString() + rail2.toString();
    }
    
    private static String railFenceDecipher(String text) {
        int mid = (text.length() + 1) / 2;
        StringBuilder result = new StringBuilder();
        
        for (int i = 0; i < mid; i++) {
            result.append(text.charAt(i));
            if (i + mid < text.length()) {
                result.append(text.charAt(i + mid));
            }
        }
        return result.toString();
    }
    
    private static void analyzeText(String text) {
        System.out.println("\n📊 Text Analysis:");
        System.out.println("Length: " + text.length() + " characters");
        System.out.println("Word count: " + text.split("\\s+").length);
        
        int letters = 0, digits = 0, spaces = 0;
        for (char c : text.toCharArray()) {
            if (Character.isLetter(c)) letters++;
            if (Character.isDigit(c)) digits++;
            if (Character.isWhitespace(c)) spaces++;
        }
        
        System.out.println("Letters: " + letters);
        System.out.println("Digits: " + digits);
        System.out.println("Spaces: " + spaces);
        System.out.println("Unique characters: " + text.chars().distinct().count());
        
        // Find most common character
        Map<Character, Integer> freq = new HashMap<>();
        for (char c : text.toCharArray()) {
            freq.put(c, freq.getOrDefault(c, 0) + 1);
        }
        
        char mostCommon = Collections.max(freq.entrySet(), Map.Entry.comparingByValue()).getKey();
        System.out.println("Most common character: '" + mostCommon + "' (" + freq.get(mostCommon) + " times)");
    }
}
