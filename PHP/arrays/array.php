<?php
session_start();

if (!isset($_SESSION["users"])) {
    $_SESSION["users"] = array();
}

if ($_SERVER["REQUEST_METHOD"] == "POST") {
    $user = array(
        "Name" => htmlspecialchars($_POST["name"]),
        "Age" => htmlspecialchars($_POST["age"]),
        "Email" => htmlspecialchars($_POST["email"]),
        "City" => htmlspecialchars($_POST["city"]),
        "Gender" => htmlspecialchars($_POST["gender"])
    );

    $_SESSION["users"][] = $user;
}

if (isset($_POST["reset"])) {
    $_SESSION["users"] = array();
}
?>

<!DOCTYPE html>
<html>
<head>
    <title>Complex PHP User Details</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            background-color: #f4f6f8;
            padding: 20px;
        }
        form {
            background: white;
            padding: 15px;
            border-radius: 10px;
            width: 350px;
            box-shadow: 0 0 10px rgba(0,0,0,0.1);
        }
        table {
            margin-top: 20px;
            border-collapse: collapse;
            width: 80%;
            background: white;
        }
        th, td {
            border: 1px solid #ccc;
            padding: 8px 12px;
            text-align: center;
        }
        th {
            background-color: #007bff;
            color: white;
        }
        h2 {
            margin-top: 30px;
        }
        .btn {
            background: #007bff;
            color: white;
            border: none;
            padding: 8px 12px;
            border-radius: 5px;
            cursor: pointer;
        }
        .btn:hover {
            background: #0056b3;
        }
        .reset {
            background: #dc3545;
        }
        .reset:hover {
            background: #b02a37;
        }
    </style>
</head>
<body>

<h1>Enter User Details</h1>

<form method="post" action="">
    <label>Name:</label><br>
    <input type="text" name="name" required><br><br>

    <label>Age:</label><br>
    <input type="number" name="age" required><br><br>

    <label>Email:</label><br>
    <input type="email" name="email" required><br><br>

    <label>City:</label><br>
    <input type="text" name="city" required><br><br>

    <label>Gender:</label><br>
    <select name="gender" required>
        <option value="">Select</option>
        <option value="Male">Male</option>
        <option value="Female">Female</option>
        <option value="Other">Other</option>
    </select><br><br>

    <button type="submit" class="btn">Add User</button>
    <button type="submit" name="reset" class="btn reset">Reset All</button>
</form>

<?php
if (!empty($_SESSION["users"])) {
    echo "<h2>All User Details</h2>";
    echo "<table>";
    echo "<tr><th>#</th><th>Name</th><th>Age</th><th>Email</th><th>City</th><th>Gender</th></tr>";
    
    $count = 1;
    foreach ($_SESSION["users"] as $user) {
        echo "<tr>";
        echo "<td>$count</td>";
        foreach ($user as $value) {
            echo "<td>$value</td>";
        }
        echo "</tr>";
        $count++;
    }
    echo "</table>";
}
?>

</body>
</html>
