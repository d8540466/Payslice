# Payslice

# 💰 Payslice: Smart Payroll Distribution Contract

## 🎯 Overview
Payslice is a smart contract that automatically splits and distributes incoming payments according to predefined percentages across different addresses for savings, loan payments, investments, and main wallet.

## ✨ Features
- 🔄 Automatic payment splitting
- 💳 Support for multiple payment destinations
- 📊 Customizable distribution percentages
- 🔐 Secure profile management
- 💼 Individual user profiles

## 📝 Contract Functions

### Profile Management
- `create-profile`: Set up a new payment distribution profile
- `update-profile`: Modify existing profile settings
- `delete-profile`: Remove payment profile
- `get-profile`: View current profile settings

### Payment Processing
- `receive-payment`: Process and distribute incoming payment
- `emergency-withdraw`: Admin function for emergency fund recovery

## 🚀 Usage Example

1. Create your payment profile:
```clarity
(contract-call? .payslice create-profile 
    'ST1SAVINGS... u30    ;; Savings address and 30%
    'ST1LOAN...   u20    ;; Loan address and 20%
    'ST1INVEST... u10    ;; Investment address and 10%
    'ST1MAIN...          ;; Main address for remaining 40%
)
```

2. Receive and auto-distribute payment:
```clarity
(contract-call? .payslice receive-payment u1000000) ;; Amount in µSTX
```

## ⚠️ Requirements
- Clarinet
- Stacks blockchain wallet
- STX tokens for transaction fees

## 🔒 Security
- Owner-only administrative functions
- Percentage validation
- Balance checks
- Profile existence validation


