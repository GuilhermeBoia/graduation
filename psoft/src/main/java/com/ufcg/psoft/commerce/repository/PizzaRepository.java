package com.ufcg.psoft.commerce.repository;

import com.ufcg.psoft.commerce.model.Pizza;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Service;

@Service
public interface PizzaRepository extends JpaRepository<Pizza, Long> {
}
